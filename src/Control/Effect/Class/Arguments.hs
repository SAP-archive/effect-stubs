{-# LANGUAGE FlexibleInstances #-}

module Control.Effect.Class.Arguments(
    Arguments(..)
) where

import           Control.Exception.Safe
import           Data.Text                     (Text)

import           Control.Monad.IO.Class

import           Control.Effect.IO.Arguments
import           Control.Effect.Stub.Arguments
import           Control.Effect.Stub.Monad

class (Monad m, MonadThrow m) => Arguments m where
  arguments :: m [Text]

instance Arguments IO where
  arguments = Control.Effect.IO.Arguments.arguments

instance (Monad m, MonadThrow m, Monoid w, HasArguments c) => Arguments (StubT c w s m) where
  arguments = Control.Effect.Stub.Arguments.arguments
