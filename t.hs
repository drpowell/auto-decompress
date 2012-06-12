#!/usr/bin/env runghc

import System.IO
import System.Environment
import qualified Data.ByteString as B
import Data.AutoDecompress

main = do
  args <- getArgs
  case args of
    ("-t":files) -> mapM_ (fileLength True) files
    files -> mapM_ (fileLength False) files

fileLength :: Bool -> FilePath -> IO ()
fileLength transform file = do
  h <- openFile file ReadMode
  h2 <- if transform then decompressHandle h else return h
  str <- B.hGetContents h2
  print $ B.length str