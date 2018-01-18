{-# LANGUAGE FlexibleInstances #-}

module Control.Effect.IO.Wait where

import           Control.Concurrent     (threadDelay)

import           Data.Hourglass

wait :: (TimeInterval i) => i -> IO ()
wait n = threadDelay $ 1000000 * (fromIntegral . toInteger . toSeconds) n