import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever, void, guard)
import Data.Time
import System.Cmd
import System.Locale
import System.IO.Error

-- Number of microseconds before status bar is refreshed.
{-
statusDelayLength :: Double
statusDelayLength = 1000000
-}

-- Frequency, action
-- The action will be wrapped so that it occurs in a new thread at the
-- given frequency and that the result is kept in STM-land.
-- TODO: Replace string w/ Text
-- TODO: Record syntax?
{-
data Task = Task Double (IO String)

tasks :: [Task]
tasks = [Task 1 getTime]
-}

main :: IO ()
main = do
  timeTVar <- atomically $ newTVar ""
  batTVar <- atomically $ newTVar ""
  forkIO $
    forever $ do
      time <- getTime
      atomically $ writeTVar timeTVar time
      threadDelay 1000000
  forkIO $
    forever $ do
      batStatus <- getBatStatus
      batCapacity <- getBatCapacity
      atomically $
        writeTVar batTVar $ formatBatStr batStatus batCapacity
      threadDelay 10000000
  forever $ do
    t <- readTVarIO timeTVar
    b <- readTVarIO batTVar
    putStatus $ b ++ " " ++ t
    threadDelay 1000000

putStatus :: String -> IO ()
putStatus status = void $ rawSystem "xsetroot" ["-name", status]

-- TODO: Where should the formatting be done?
getTime :: IO String
getTime = formatT <$> getZonedTime
  where
    formatT = formatTime defaultTimeLocale "%T"

-- TODO: Instead of checking every so often, this could be informed
-- by ACPI events.
getBatStatus :: IO String
getBatStatus = do
  let file = "/sys/class/power_supply/BAT0/status"
  e <- tryJust (guard . isDoesNotExistError) (readFile file)
  case either (const "") strip e of
    "Full"        -> return "(=)"
    "Charging"    -> return "(+)"
    "Discharging" -> return "(-)"
    _             -> return "(?)"

-- TODO: Instead of checking every so often, this could be informed
-- by ACPI events.
getBatCapacity :: IO String
getBatCapacity = do
  let file = "/sys/class/power_supply/BAT0/capacity"
  e <- tryJust (guard . isDoesNotExistError) (readFile file)
  return $ either (const "") (\x -> strip x ++ "%") e

formatBatStr :: String -> String -> String
formatBatStr s c = c ++ " " ++ s

strip :: String -> String
strip = filter (/= '\n')
