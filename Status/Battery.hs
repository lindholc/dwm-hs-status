module Status.Battery (getBatStatus, getBatCapacity) where

import Status.Util (strip)

import Control.Exception
import Control.Monad (guard)
import System.IO.Error

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
