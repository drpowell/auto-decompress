module Data.AutoDecompress
    ( decompressDefs
    , withDecompressFile, decompressFile --, decompressHandle
    ) where

import Data.AutoDecompress.Transform
import qualified Data.ByteString as B
import System.IO (Handle, openFile, withFile, hClose, IOMode(..))

decompressDefs =
    [ TransformDef { name = "gzip"
                   , checkType = checkMagic (B.pack [0x1f, 0x8b])
                   , transformers = [Transformer "gunzip" ["-d", "-c"]]
                   }
    , TransformDef { name = "bzip2"
                   , checkType = checkMagic (B.pack [0x42, 0x5a, 0x68])  -- "BZh"
                   , transformers = [ Transformer "bunzip" ["-d", "-c"]
                                    , Transformer "bunzip2" ["-d", "-c"]
                                    ]
                   }
    , TransformDef { name = "funzip"
                   , checkType = checkMagic (B.pack [0x50, 0x4b, 0x03, 0x04])  -- "PK\003\004"
                   , transformers = [Transformer "funzip" []]
                   }
    , TransformDef { name = "RAW"
                   , checkType = \_ _ -> True
                   , transformers = [NoTransform]
                   }
    ]

withDecompressFile :: FilePath -> (Handle -> IO r) -> IO r
withDecompressFile file action = withTransformFile decompressDefs file action


decompressFile :: FilePath -> IO Handle
decompressFile file = transformFile decompressDefs file

-- decompressHandle :: Handle -> IO Handle
-- decompressHandle hndl = transformHandle decompressDefs hndl
