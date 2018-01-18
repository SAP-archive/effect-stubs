{-# LANGUAGE FlexibleInstances #-}

module Control.Effect.Class.Console(
   Console(..)
) where

import           Data.ByteString             (ByteString)

import           Control.Exception.Safe

import           Control.Monad.IO.Class

import           Control.Effect.IO.Console
import           Control.Effect.Stub.Console
import           Control.Effect.Stub.Monad

class (Monad m, MonadThrow m) => Console m where
  readStdin   :: m ByteString
  writeStdout :: ByteString -> m ()
  writeStderr :: ByteString -> m ()

instance Console IO where
  readStdin = Control.Effect.IO.Console.readStdin
  writeStdout = Control.Effect.IO.Console.writeStdout
  writeStderr = Control.Effect.IO.Console.writeStderr

instance (Monad m, MonadThrow m, HasStdin r, HasStdout w, HasStderr w) => Console (StubT r w s m) where
  readStdin = Control.Effect.Stub.Console.readStdin
  writeStdout = Control.Effect.Stub.Console.writeStdout
  writeStderr = Control.Effect.Stub.Console.writeStderr
