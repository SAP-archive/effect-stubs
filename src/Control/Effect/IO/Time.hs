module Control.Effect.IO.Time where

import           Time.System
import           Time.Types

currentTime :: IO Elapsed
currentTime = timeCurrent
