module Control.Effect.Stub.Console(
    HasStdin(..)
  , readStdin
  , HasStdout(..)
  , writeStdout
  , HasStderr(..)
  , writeStderr
) where

import           Data.ByteString              (ByteString)

import           Control.Exception.Safe
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

class HasStdin a where
  asStdin :: a -> ByteString
  asStdin = const ""

class (Monoid a) => HasStdout a where
  asStdout :: ByteString -> a
  asStdout = mempty

class (Monoid a) => HasStderr a where
  asStderr :: ByteString -> a
  asStderr = mempty

readStdin :: (Monad m, MonadThrow m, MonadReader c m, HasStdin c) => m ByteString
readStdin = asks asStdin

writeStdout :: (Monad m, MonadThrow m, MonadWriter w m, HasStdout w) => ByteString -> m ()
writeStdout s = tell $ asStdout s

writeStderr :: (Monad m, MonadThrow m, MonadWriter w m, HasStderr w) => ByteString -> m ()
writeStderr s = tell $ asStderr s
