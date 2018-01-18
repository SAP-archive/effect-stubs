{-# LANGUAGE FlexibleInstances #-}

module Control.Effect.Class.FileSystem(
    FileSystem(..)
) where

import           Control.Exception.Safe
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as ByteString
import           Data.Text                      (Text)
import qualified Data.Text                      as Text

import           Control.Monad.IO.Class

import           Control.Effect.IO.FileSystem
import           Control.Effect.Stub.FileSystem
import           Control.Effect.Stub.Monad

class (Monad m, MonadThrow m) => FileSystem m where
  readFile :: Text -> m ByteString

instance FileSystem IO where
  readFile = Control.Effect.IO.FileSystem.readFile

instance (Monad m, MonadThrow m, HasFiles s, Monoid w) => FileSystem (StubT c w s m) where
  readFile = Control.Effect.Stub.FileSystem.readFile
