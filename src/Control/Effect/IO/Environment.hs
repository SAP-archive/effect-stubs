module Control.Effect.IO.Environment where

import           Control.Exception.Safe
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HashMap
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           System.Environment

environment :: IO (HashMap Text Text)
environment = getEnvironment >>= pure . HashMap.fromList . fmap (\(k,v) -> (Text.pack k, Text.pack v))
