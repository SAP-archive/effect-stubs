module Control.Effect.Stub(
    module Has
  , StubT(..)
  , runStubT
) where

import           Control.Effect.Stub.Arguments   as Has (HasArguments (..))
import           Control.Effect.Stub.Console     as Has (HasStderr (..),
                                                         HasStdin (..),
                                                         HasStdout (..))
import           Control.Effect.Stub.Environment as Has (HasEnvironment (..))
import           Control.Effect.Stub.FileSystem  as Has (HasFiles (..))
import           Control.Effect.Stub.Monad       (StubT (..), runStubT)
import           Control.Effect.Stub.Time        as Has (HasTime (..),
                                                         HasTimeline (..),
                                                         MonadFakeTime (..))
import           Control.Effect.Stub.Wait        as Has (HasWaitCount (..))
