{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad (forever, sequence)
import Data.Time
import System.Cmd
import System.Locale

data Status = Status {_sTime :: String}

instance Show Status where
  show (Status t) = show t

-- How does Template Haskell work?
makeLenses ''Status

-- TODO: Any way to do this automatically?
emptyStatus :: Status
emptyStatus = Status ""

-- TODO: Need to run as soon as the program starts, then wait for the next
-- time. Don't want to start by waiting.
main :: IO ()
main = do
  statusTVar <- atomically $ newTVar (Status "")
  forever $ do
    timeThread <- forkIO $ do
      -- These lines can be combined w/ an applicative functor or
      -- something like that.
      time <- getTime
      atomically $ modifyTVar' statusTVar $ (\x -> x & sTime .~ time)
    status <- atomically $ readTVar statusTVar
    putStatus $ show status

putStatus :: String -> IO ()
putStatus status = rawSystem "xsetroot" ["-name", status] >> return ()

-- TODO: Where should the formatting be done?
getTime :: IO String
getTime = formatT <$> getZonedTime
  where
    formatT = formatTime defaultTimeLocale "%T"
