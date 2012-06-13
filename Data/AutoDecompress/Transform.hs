{-# LANGUAGE DeriveDataTypeable #-}

module Data.AutoDecompress.Transform
    ( transformFile, transformFile', withTransformFile
    , transformHandle', withTransformHandle
    , cleanupTransform
    , TransformDef(..), Transformer(..)
    , checkMagic, runPipe
    ) where

import Prelude hiding (catch)
import System.IO (stdin, Handle, openFile, hClose, hSetBuffering, withFile, hLookAhead, BufferMode(..), IOMode(..))
import System.IO.Error (isEOFError)
import System.Exit (ExitCode(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import System.Process (waitForProcess, ProcessHandle, terminateProcess, createProcess, CreateProcess(..), proc, StdStream(..))
import System.Posix.IO (createPipe, fdToHandle)
import Control.Concurrent (forkIO)
import Network.Socket.Splice (hSplice)
import Control.Exception (throw, catch, bracket, ErrorCall(..), fromException, Exception)
import Data.Typeable

hdrLen = 8 :: Int

data TransformDef = TransformDef { checkType :: Maybe FilePath -> ByteString -> Bool
                                 , transformers :: [Transformer]
                                 , name :: String
                                 }

-- | A Transformer should take a filename as the next parameter, and if none given should read from stdin
data Transformer = Transformer { prog :: FilePath
                               , args :: [String]
                               }
                 | NoTransform

withTransformFile :: [TransformDef] -> FilePath -> (Handle -> IO r) -> IO r
withTransformFile defs file action = withAndCleanup (transformFile' defs file) action

transformFile :: [TransformDef] -> FilePath -> IO Handle
transformFile defs file = snd `fmap` transformFile' defs file

-- | Transform a given file according to the given list of 'TransformDef`.  Note that if the
-- filename is empty or "-" this will read from stdin (and thus use 'transformHandle'`)
transformFile' :: [TransformDef] -> FilePath -> IO (Maybe ProcessHandle, Handle)
transformFile' defs file
    | file == "" || file == "-" = transformHandle' defs stdin
    | otherwise                 = do
        hdr <- withFile file ReadMode $ \hndl -> B.hGet hndl hdrLen
        case filter (\def -> checkType def Nothing hdr) defs of
          (def:_) -> tryEachTransform def (transformers def)
          [] -> error "No matching transformer"
  where
    tryEachTransform def [] = error $ "Unable to find transformer for '"++name def++"'"
    tryEachTransform def (t:ts) = catch (runPipe t file)
                                        (\(UnableToRun _) -> tryEachTransform def ts)


cleanupTransform :: Maybe ProcessHandle -> IO ()
cleanupTransform p = maybe (return ()) terminateProcess p

withTransformHandle :: [TransformDef] -> Handle -> (Handle -> IO c) -> IO c
withTransformHandle defs hndl action = withAndCleanup (transformHandle' defs hndl) action

-- | Limitation that if more than 1 decompressor, will attempt to run with empty stdin to see if it exists
transformHandle' :: [TransformDef] -> Handle -> IO (Maybe ProcessHandle, Handle)
transformHandle' defs hndl = do
    hdr <- B.hGet hndl hdrLen
    case filter (\def -> checkType def Nothing hdr) defs of
      (def:_) -> tryOneTransform def (transformers def) hdr
      [] -> error "No matching transformer"
  where
    tryOneTransform def [] _       = error $ "Unable to find transformer for '"++name def++"'"
    tryOneTransform _ [t] hdr      = runPipeOnHandle t hdr hndl
    tryOneTransform def (t:ts) hdr = do ok <- canRunPipe t
                                        if ok
                                          then runPipeOnHandle t hdr hndl
                                          else tryOneTransform def ts hdr


withAndCleanup :: IO (Maybe ProcessHandle, Handle) -> (Handle -> IO c) -> IO c
withAndCleanup setupHandle action = bracket setupHandle
                                            (\(p,h) -> do hClose h
                                                          maybe (return ()) terminateProcess p)
                                            (\(_,h) -> action h)


checkMagic :: ByteString -> t -> ByteString -> Bool
checkMagic magic _ bs = magic `B.isPrefixOf` bs

data UnableToRun = UnableToRun String
     deriving (Show, Typeable)
instance Exception UnableToRun


-- | Create two handles connected via a pipe
createPipeH :: IO (Handle, Handle)
createPipeH = do (a,b) <- createPipe
                 aH <- fdToHandle a
                 bH <- fdToHandle b
                 return (aH,bH)

-- | Check if the given transform is able to run.  The check is to run the program
-- giving it nothing on stdin - if the return code is 127 assume the program is invalid
-- Quite a hack, but only used when reading from a handle
canRunPipe :: Transformer -> IO Bool
canRunPipe NoTransform = return True
canRunPipe (Transformer prog args) = do
    (Just inH, Just outH, Just errH, phndl) <-
        createProcess $ (proc prog args) { std_in = CreatePipe
                                         , std_out = CreatePipe
                                         , std_err = CreatePipe}
    hClose inH
    hClose errH
    eCode <- waitForProcess phndl
    hClose outH
    return $ eCode /= ExitFailure 127

onBadExit :: ProcessHandle -> (ExitCode -> IO ()) -> IO ()
onBadExit phndl action = do eCode <- waitForProcess phndl
                            case eCode of  -- Ignore success, and SIGTERM (we send it to ourselves to cleanup)
                              ExitSuccess -> return ()
                              ExitFailure 15 -> return ()
                              ExitFailure _ -> action eCode

reaper :: String -> ProcessHandle -> IO ()
reaper prog phndl = onBadExit phndl (\eCode -> error $ "Pipe failed ("++prog++") : " ++ show eCode)


runPipe :: Transformer -> FilePath -> IO (Maybe ProcessHandle, Handle)
runPipe NoTransform file = openFile file ReadMode >>= \h -> return (Nothing, h)
runPipe (Transformer prog args) file = do
    (Just inH, Just outH, _, phndl) <- createProcess $ (proc prog (args ++ [file])) {std_in = CreatePipe, std_out = CreatePipe}
    hClose inH

    exited <- checkValid phndl outH

    if not exited
       then do forkIO (reaper prog phndl)
               return (Just phndl, outH)
       else return (Nothing, outH)
  where
    checkValid phndl rdr = catch (hLookAhead rdr >> return False)
                                  (\e -> if isEOFError e
                                           then do onBadExit phndl (throw $ UnableToRun prog)
                                                   return True
                                           else throw e)


runPipeOnHandle :: Transformer -> ByteString -> Handle -> IO (Maybe ProcessHandle, Handle)
runPipeOnHandle NoTransform hdr hndl = pushbackOntoHandle hdr hndl >>= \h -> return (Nothing, h)
runPipeOnHandle (Transformer prog args) hdr hndl = do
    pushedHndl <- pushbackOntoHandle hdr hndl
    (_, Just outH, _, phndl) <- createProcess $ (proc prog args) {std_in = UseHandle pushedHndl, std_out = CreatePipe}
    forkIO (reaper prog phndl)

    return (Just phndl, outH)

pushbackOntoHandle :: ByteString -> Handle -> IO Handle
pushbackOntoHandle hdr hndl = do
    (rdrH, wtrH) <- createPipeH
    hSetBuffering wtrH NoBuffering
    hSetBuffering rdrH NoBuffering
    B.hPut wtrH hdr

    -- NoBuffering is a requirement of the splice function
    hSetBuffering hndl NoBuffering
    forkIO $ catch
                (hSplice 4096 hndl wtrH)
                (\e -> do hClose wtrH
                          -- hClose hndl
                          case fromException e of
                            Just (ErrorCall _) -> return ()
                            _ -> throw e)

    return rdrH

