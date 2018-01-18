{-# LANGUAGE FlexibleInstances #-}

module Control.Effect.Class.Environment(
    Environment(..)
) where

import           Control.Exception.Safe
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as HashMap
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           System.Environment

import           Control.Monad.IO.Class

import           Control.Effect.IO.Environment
import           Control.Effect.Stub.Environment
import           Control.Effect.Stub.Monad

class (Monad m, MonadThrow m) => Environment m where
  environment :: m (HashMap Text Text)

instance Environment IO where
  environment = Control.Effect.IO.Environment.environment

instance (Monad m, MonadThrow m, Monoid w, HasEnvironment s) => Environment (StubT c w s m) where
  environment = Control.Effect.Stub.Environment.environment
