auto-decompress
===============

Haskell library to automatically decompress a file when opening.

Example, to print the uncompressed length of the file "test.gz":

    import System.IO
    import Data.AutoDecompress

    main = withDecompressFile "test.gz" $ \h -> do
              str <- hGetContents h
              print $ length str

How it works
============

The module `Data.AutoDecompress.Transform` provides functions to
transform files based on the their "magic number".  The convenience
module `Data.AutoDecompress` defines decompressors for gzip (gunzip),
bzip2 (bunzip2), zip (funzip).  You can use the
`Data.AutoDecompress.Transform` module directly if you wish to use
different decompressors.