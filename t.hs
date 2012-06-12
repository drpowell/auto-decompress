#!/usr/bin/env runghc

import System.IO
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.AutoDecompress
import Control.Concurrent

main = do
  args <- getArgs
  case args of
    ("-t":files) -> mapM_ (fileLength True) files
    ("-l":files) -> lotsOfFiles (head files)
    files -> mapM_ (fileLength False) files

fileLength :: Bool -> FilePath -> IO ()
fileLength transform file = do
  h <- if transform
         then decompressFile file
         else openFile file ReadMode
  str <- B.hGetContents h
  print $ B.length str


lotsOfFiles :: FilePath -> IO ()
lotsOfFiles file = do
  lens <- mapM (\_ -> withDecompressFile file (\h -> do str <- BL.hGetContents h
                                                        let c = BL.take 1 str
                                                        c `seq` return c
                                              )
               ) [1..100]
  print lens
  threadDelay $ 30 * 1000 * 1000

