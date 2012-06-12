{-# LANGUAGE DeriveDataTypeable #-}

module Data.AutoDecompress.Transform
    ( transformHandle, transformFile, withTransformFile
    , TransformDef(..), Transformer(..)
    , checkMagic, runPipe, tryManyRunPipe
    ) where

import Prelude hiding (catch)
import System.IO (Handle, openFile, hClose, hSetBuffering, withFile, hLookAhead, BufferMode(..), IOMode(..))
import System.IO.Error (isEOFError)
import System.Exit (ExitCode(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import System.Process (runProcess, waitForProcess)
import System.Posix.IO (createPipe, fdToHandle)
import Control.Concurrent (forkIO)
import Network.Socket.Splice (hSplice, tryWith)
import Control.Exception (throw, catch, bracket, onException, ErrorCall(..), fromException, Exception)
import Control.Monad (when)
import Data.Typeable

hdrLen = 8 :: Int

data TransformDef = TransformDef { checkType :: Maybe FilePath -> ByteString -> Bool
                                 , transformer :: Transformer
                                 }

data Transformer = HandleTransformer (Handle -> IO Handle)

withTransformFile :: [TransformDef] -> FilePath -> (Handle -> IO r) -> IO r
withTransformFile defs file action = withFile file ReadMode (\h -> bracket (transformHandle defs h) hClose (action))

transformFile :: [TransformDef] -> FilePath -> IO Handle
transformFile defs file = openFile file ReadMode >>= transformHandle defs

transformHandle :: [TransformDef] -> Handle -> IO Handle
transformHandle defs hndl = do
  hdr <- B.hGet hndl hdrLen
  case filter (\def -> checkType def Nothing hdr) defs of
    (def:_) -> doTransform hdr hndl (transformer def)
    [] -> error "No match"

checkMagic :: ByteString -> t -> ByteString -> Bool
checkMagic magic _ bs = magic `B.isPrefixOf` bs

data UnableToRun = UnableToRun String
     deriving (Show, Typeable)
instance Exception UnableToRun

runPipe :: FilePath -> [String] -> Handle -> IO Handle
runPipe prog args hndl = do
  (rdrH, wtrH) <- createPipeH
  phndl <- runProcess prog args Nothing Nothing (Just hndl) (Just wtrH) Nothing
  exited <- catch (hLookAhead rdrH >> return False)
                  (\e -> if isEOFError e
                           then do eCode <- waitForProcess phndl
                                   when (isFail eCode) $
                                       throw $ UnableToRun prog
                                   return True
                           else throw e)
  when (not exited) $ forkIO (reaper phndl) >> return ()
  return rdrH
 where
   isFail ExitSuccess = False
   isFail _ = True

   reaper phndl = do eCode <- waitForProcess phndl
                     when (isFail eCode) $
                         error ("Pipe failed ("++prog++") : "++show eCode)

tryManyRunPipe :: String -> [Handle -> IO Handle] -> Handle -> IO Handle
tryManyRunPipe typ [] _ = error $ "No program available to transform '"++typ++"'"
tryManyRunPipe typ (pipe:pipes) hndl = do
  catch (pipe hndl)
        (\(UnableToRun _) -> tryManyRunPipe typ pipes hndl)

createPipeH :: IO (Handle, Handle)
createPipeH = do (a,b) <- createPipe
                 aH <- fdToHandle a
                 bH <- fdToHandle b
                 return (aH,bH)

doTransform hdr hndl (HandleTransformer trnsfmer) = do
  (rdrH, wtrH) <- createPipeH
  B.hPut wtrH hdr

  -- NoBuffering is a requirement of the splice function
  hSetBuffering wtrH NoBuffering
  hSetBuffering hndl NoBuffering
  forkIO $ catch
              (hSplice 40960 hndl wtrH)
              (\e -> do hClose wtrH
                        hClose hndl
                        case fromException e of
                          Just (ErrorCall _) -> return ()
                          _ -> throw e)

  onException (trnsfmer rdrH)  (hClose rdrH)
