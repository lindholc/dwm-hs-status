module Status.Time (getTime) where

import Control.Applicative
import Data.Time
import System.Locale

getTime :: IO String
getTime = formatT <$> getZonedTime
  where
    formatT = formatTime defaultTimeLocale "%T"
