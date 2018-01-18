module Control.Effect.IO.Console where

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as ByteString

import           Control.Exception.Safe
import           System.IO              (stderr)

readStdin :: IO ByteString
readStdin = ByteString.getContents

writeStdout :: ByteString -> IO ()
writeStdout = ByteString.putStr

writeStderr :: ByteString -> IO ()
writeStderr = ByteString.hPutStr stderr
