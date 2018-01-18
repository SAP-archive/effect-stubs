module Control.Effect.Stub.Arguments(
    HasArguments(..)
  , arguments
) where

import           Data.Text                    (Text)

import           Control.Exception.Safe
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

class HasArguments a where
  asArguments :: a -> [Text]
  asArguments = const []

arguments :: (Monad m, MonadThrow m, MonadReader c m, HasArguments c) => m [Text]
arguments = asks asArguments
