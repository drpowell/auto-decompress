{-# LANGUAGE DeriveDataTypeable #-}

module Data.AutoDecompress.Transform
    ( transformFile, withTransformFile
    , TransformDef(..), Transformer(..)
    , checkMagic, runPipe
    ) where

import Prelude hiding (catch)
import System.IO (Handle, openFile, hClose, hSetBuffering, withFile, hLookAhead, BufferMode(..), IOMode(..))
import System.IO.Error (isEOFError)
import System.Exit (ExitCode(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import System.Process (runProcess, waitForProcess, ProcessHandle, terminateProcess)
import System.Posix.IO (createPipe, fdToHandle)
import Control.Concurrent (forkIO)
import Network.Socket.Splice (hSplice, tryWith)
import Control.Exception (throw, catch, bracket, onException, ErrorCall(..), fromException, Exception)
import Control.Monad (when)
import Data.Typeable

hdrLen = 8 :: Int

data TransformDef = TransformDef { checkType :: Maybe FilePath -> ByteString -> Bool
                                 , transformers :: [Transformer]
                                 , name :: String
                                 }

data Transformer = Transformer { prog :: FilePath
                               , args :: [String]
                               }
                 | NoTransform

withTransformFile :: [TransformDef] -> FilePath -> (Handle -> IO r) -> IO r
withTransformFile defs file action = bracket (transformFile' defs file)
                                             (\(p,h) -> do hClose h
                                                           maybe (return ()) terminateProcess p)
                                             (\(_,h) -> action h)

transformFile :: [TransformDef] -> FilePath -> IO Handle
transformFile defs file = snd `fmap` transformFile' defs file

transformFile' :: [TransformDef] -> FilePath -> IO (Maybe ProcessHandle, Handle)
transformFile' defs file = do
    hdr <- withFile file ReadMode $ \hndl -> B.hGet hndl hdrLen
    case filter (\def -> checkType def Nothing hdr) defs of
      (def:_) -> tryEachTransform def (transformers def)
  where
    tryEachTransform def [] = error $ "Unable to find transformer for '"++name def++"'"
    tryEachTransform def (t:ts) = catch (runPipe t file)
                                        (\(UnableToRun _) -> tryEachTransform def ts)


{-
-- FIXME Check each "pipe" can run
transformHandle :: [TransformDef] -> Handle -> IO Handle
transformHandle defs hndl = do
  hdr <- B.hGet hndl hdrLen
  case filter (\def -> checkType def Nothing hdr) defs of
    (def:_) -> tryEachTransform (transformers def) hdr
    [] -> error "No match"
 where
   tryEachTransform [] _ = error $ "No program available to transform"
   tryEachTransform (t:ts) hdr = catch (doTransform hdr hndl t)
                                       (\(UnableToRun _) -> tryEachTransform ts hdr)

-}

checkMagic :: ByteString -> t -> ByteString -> Bool
checkMagic magic _ bs = magic `B.isPrefixOf` bs

data UnableToRun = UnableToRun String
     deriving (Show, Typeable)
instance Exception UnableToRun

{-
canRunPipe :: FilePath -> [String] -> IO Bool
canRunPipe = do
  (rdrH, wtrH) <- createPipeH
  catch (do phndl <- runProcess prog args Nothing Nothing (Just hndl) (Just wtrH) Nothing
            hClose wtrH
            eCode <- waitForProcess phndl
            hClose rdrH
            return $ eCode == ExitFailure 127
        ) ()
-}

runPipe :: Transformer -> FilePath -> IO (Maybe ProcessHandle, Handle)
runPipe NoTransform file = openFile file ReadMode >>= \h -> return (Nothing, h)
runPipe (Transformer prog args) file = do
  (rdrH, wtrH) <- createPipeH
  phndl <- runProcess prog (args ++ [file]) Nothing Nothing Nothing (Just wtrH) Nothing

  exited <- checkValid phndl rdrH

  if not exited
     then do forkIO (reaper phndl)
             return (Just phndl, rdrH)
     else return (Nothing, rdrH)
 where
   isFail ExitSuccess = False
   isFail _ = True

   onBadExit phndl action = do eCode <- waitForProcess phndl
                               case eCode of  -- Ignore success, and SIGTERM (we send it to ourselves to cleanup)
                                 ExitSuccess -> return ()
                                 ExitFailure 15 -> return ()
                                 ExitFailure _ -> action eCode

   checkValid phndl rdr = catch (hLookAhead rdr >> return False)
                                 (\e -> if isEOFError e
                                          then do onBadExit phndl (throw $ UnableToRun prog)
                                                  return True
                                          else throw e)

   reaper phndl = onBadExit phndl (\eCode -> error $ "Pipe failed ("++prog++") : " ++ show eCode)

createPipeH :: IO (Handle, Handle)
createPipeH = do (a,b) <- createPipe
                 aH <- fdToHandle a
                 bH <- fdToHandle b
                 return (aH,bH)

{-

doTransform hdr hndl (HandleTransformer trnsfmer) = do
  (rdrH, wtrH) <- createPipeH
  hSetBuffering wtrH NoBuffering
  hSetBuffering rdrH NoBuffering
  B.hPut wtrH hdr
  hClose wtrH

  pipeHndl <- onException (trnsfmer rdrH)  (hClose rdrH)

  -- NoBuffering is a requirement of the splice function
  hSetBuffering hndl NoBuffering
  forkIO $ catch
              (hSplice 40960 hndl wtrH)
              (\e -> do hClose wtrH
                        hClose hndl
                        case fromException e of
                          Just (ErrorCall _) -> return ()
                          _ -> throw e)

  return pipeHndl

-}