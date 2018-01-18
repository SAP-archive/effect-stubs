{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Effect.Stub.Wait(
    HasWaitCount(..)
  , wait
) where

import           Control.Exception.Safe
import           Control.Monad.State
import           Control.Monad.Writer         hiding ((<>))

import Control.Effect.Stub.Monad
import Control.Effect.Stub.Time

import           Data.HashMap.Strict          as HashMap
import           Data.HashMap.Strict          (HashMap)
import           Data.Hourglass
import           Data.Maybe

class (Monoid a) => HasWaitCount a where
  asWaitCount :: TimeInterval i => i -> a
  asWaitCount = mempty

wait :: (Monad m, MonadThrow m, MonadWriter w m, HasWaitCount w, MonadState s m, MonadFakeTime s m, HasTimeline s, TimeInterval i) => i -> m ()
wait n = do
  tick n
  tell $ asWaitCount n
