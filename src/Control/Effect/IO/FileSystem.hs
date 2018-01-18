module Control.Effect.IO.FileSystem where

import           Control.Exception.Safe
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as ByteString
import           Data.Text              (Text)
import qualified Data.Text              as Text

readFile :: Text -> IO ByteString
readFile = ByteString.readFile . Text.unpack
