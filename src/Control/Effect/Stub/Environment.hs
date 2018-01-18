module Control.Effect.Stub.Environment(
    HasEnvironment(..)
  , environment
) where


import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HashMap
import           Data.Text                    (Text)

import           Control.Exception.Safe
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

class HasEnvironment a where
  asEnvironment :: a -> HashMap Text Text
  asEnvironment = const HashMap.empty

environment :: (Monad m, MonadThrow m, MonadState s m, HasEnvironment s) => m (HashMap Text Text)
environment = gets asEnvironment
