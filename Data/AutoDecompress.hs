module Data.AutoDecompress
    ( decompressDefs
    , withDecompressFile, decompressFile, decompressHandle
    ) where

import Data.AutoDecompress.Transform
import qualified Data.ByteString as B
import System.IO (Handle, openFile, withFile, hClose, IOMode(..))

decompressDefs =
    [ TransformDef { checkType = checkMagic (B.pack [0x1f, 0x8b])
                   , transformer = HandleTransformer (runPipe "gunzip" ["-d", "-c"])
                   }
    , TransformDef { checkType = checkMagic (B.pack [0x42, 0x5a, 0x68])  -- "BZh"
                   , transformer = HandleTransformer $ tryManyRunPipe "bzip"
                                           [runPipe "bunzip" ["-d", "-c"]
                                           ,runPipe "bunzip2" ["-d", "-c"]
                                           ]
                   }
    , TransformDef { checkType = checkMagic (B.pack [0x50, 0x4b, 0x03, 0x04])  -- "PK\003\004"
                   , transformer = HandleTransformer (runPipe "funzip" [])
                   }
    , TransformDef { checkType = \_ _ -> True
                   , transformer = HandleTransformer (return . id)
                   }
    ]

withDecompressFile :: FilePath -> (Handle -> IO r) -> IO r
withDecompressFile file action = withTransformFile decompressDefs file action


decompressFile :: FilePath -> IO Handle
decompressFile file = transformFile decompressDefs file

decompressHandle :: Handle -> IO Handle
decompressHandle hndl = transformHandle decompressDefs hndl
