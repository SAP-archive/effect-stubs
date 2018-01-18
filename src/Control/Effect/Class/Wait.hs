{-# LANGUAGE FlexibleInstances #-}

module Control.Effect.Class.Wait(
    Wait(..)
  , WaitConfig(..)
  , Retry(..)
  , Timeout(..)
  , waitFor
) where

import           Control.Concurrent        (threadDelay)
import           Control.Exception.Safe
import           Control.Monad.Trans

import           Data.Hourglass
import           Data.Maybe
import           Data.Semigroup
import           Data.Text

import           Control.Monad.IO.Class

import           Control.Effect.IO.Wait
import           Control.Effect.Stub.Monad
import           Control.Effect.Stub.Time
import           Control.Effect.Stub.Wait

class (Monad m, MonadThrow m) => Wait m where
  wait :: (TimeInterval i) => i -> m ()

instance Wait IO where
  wait = Control.Effect.IO.Wait.wait

instance (Monad m, MonadThrow m, Monoid w, HasWaitCount w, HasTime s, HasTimeline s) => Wait (StubT r w s m) where
  wait = Control.Effect.Stub.Wait.wait

data TimeInterval i => WaitConfig i = WaitConfig {
    retries  :: Retry
  , interval :: i
  , message  :: Text
}

data Retry =
    Unlimited
  | Retry {
      count :: Int
    }

instance Eq Retry where
  Unlimited == Unlimited = True
  (Retry n) == (Retry m) = n == m

data Timeout = Timeout Text deriving (Typeable, Show, Eq)

instance Exception Timeout

waitFor :: (Wait m, MonadThrow m, TimeInterval i) => WaitConfig i -> m (Maybe a) -> (Maybe a -> Bool) -> m (Maybe a)
waitFor waitConfig next predicate =
  go waitConfig 0
    where
      go waitConfig n = do
        mResource <- next
        if predicate mResource
          then pure mResource
          else
            case retries waitConfig of
              Retry m | m < n -> throwM $ Timeout $ "Waiting for " <> message waitConfig
              _ -> do
                Control.Effect.Class.Wait.wait $ interval waitConfig
                go waitConfig (n + 1)
