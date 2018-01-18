{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Control.Effect.Stub.Monad(
    StubT(..)
  , runStubT
) where


import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad.Base
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.RWS.Lazy
import           Control.Monad.State
import           Control.Monad.Trans
import           Control.Monad.Trans.Control
import           Control.Monad.Writer

newtype StubT r w s m a = StubT (RWST r w s m a)
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadBase b
    , MonadIO
    , MonadReader r
    , MonadState s
    , MonadWriter w
    , MonadThrow
    , MonadCatch
    , MonadMask)

instance (Monoid w) => MonadTrans (StubT r w s) where
  lift = StubT . lift

instance (Monoid w) => MonadTransControl (StubT r w s) where
  type StT (StubT r w s) a = StT (RWST r w s) a
  liftWith = defaultLiftWith StubT (\(StubT x) -> x)
  restoreT = defaultRestoreT StubT

instance (Monoid w, MonadBaseControl b m) => MonadBaseControl b (StubT r w s m) where
  type StM (StubT r w s m) a = ComposeSt (StubT r w s) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

runStubT :: (Monad m) => r -> s -> StubT r w s m a -> m (a, s, w)
runStubT r s (StubT stubs) = runRWST stubs r s
