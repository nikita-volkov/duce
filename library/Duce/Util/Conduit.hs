module Duce.Util.Conduit where

import Conduit
import qualified Data.Conduit.Lzma as LzmaConduit
import Duce.Prelude

processFile :: FilePath -> ConduitT ByteString Void IO r -> IO r
processFile path conduit =
  withSourceFile path $ \source ->
    runConduit $ source .| conduit

processLzmaFile :: FilePath -> ConduitT ByteString Void IO r -> IO r
processLzmaFile path conduit =
  processFile path $
    LzmaConduit.decompress Nothing .| conduit
