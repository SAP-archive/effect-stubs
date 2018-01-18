module Control.Effect.IO.Arguments where

import           Control.Exception.Safe
import           Data.Text              (Text)
import           Data.Text              as Text
import           System.Environment

arguments :: IO [Text]
arguments = getArgs >>= pure . fmap Text.pack
