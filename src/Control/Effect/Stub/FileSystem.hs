module Control.Effect.Stub.FileSystem(
    HasFiles(..)
  , readFile
) where

import Prelude hiding (readFile)

import           Control.Applicative
import           Control.Monad.State
import           Data.Maybe

import           Control.Exception.Safe
import           Data.ByteString              (ByteString)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HashMap
import           Data.Text                    (Text)

class HasFiles a where
  asFiles :: a -> HashMap Text ByteString
  asFiles = const HashMap.empty

readFile :: (Monad m, MonadThrow m, MonadState s m, HasFiles s) => Text -> m ByteString
readFile path = do
  files <- gets asFiles
  --TODO *** Exception: <path>: openFile: does not exist (No such file or directory)
  pure $ fromJust $ path `HashMap.lookup` files
