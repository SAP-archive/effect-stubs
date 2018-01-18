{-# LANGUAGE FlexibleInstances #-}

module Control.Effect.Class.Time(
    Time(..)
) where

import           Time.System
import           Time.Types

import           Control.Monad.IO.Class

import           Control.Effect.IO.Time
import           Control.Effect.Stub.Monad
import           Control.Effect.Stub.Time

class (Monad m) => Time m where
  currentTime :: m Elapsed

instance Time IO where
  currentTime = Control.Effect.IO.Time.currentTime

instance (Monad m, Monoid w, HasTime s) => Time (StubT r w s m) where
  currentTime = Control.Effect.Stub.Time.currentTime
