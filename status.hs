import Control.Monad (forever, sequence)
import Data.ByteString.Char8 (unpack)
import Data.Conduit
import Data.Conduit.Binary (sourceFile, lines)
import Data.Conduit.List (consume)
import Data.Time
import Prelude hiding (lines)
import System.Cmd
import System.Locale
import System.Timer.Updatable

-- What if this was done by lazily constructing a string for the status
-- using the functions that get the data? That way, you can do all the
-- formatting easily, and then each time it tries to make the string, it
-- calls the functions and get the updated values.

-- TODO: Need to run as soon as the program starts, then wait for the next
-- time. Don't want to start by waiting.
main :: IO ()
main = forever $ serial (sequence thingsToDo >>= (\s ->
  rawSystem "xsetroot" ["-name", unwords s])) (10^6) >>=
    (\t -> waitIO t) >> return ()

-- All the things used to construct the string will most likely
-- be in the IO monad.
thingsToDo :: [IO String]
thingsToDo = [getBattery, getBatteryStatus, getTime]

-- TODO: Could this be done more idiomatically? (lift or something?)
-- TODO: Format so the time is only hours and minutes.
getTime :: IO String
getTime = getZonedTime >>=
  (\t -> return $ formatTime defaultTimeLocale "%T" t)

-- TODO: How do I deal with ByteStrings?
getBattery :: IO String
getBattery = (runResourceT $ sourceFile battFile $= lines $$ consume) >>=
  (\t -> return $ (unpack $ t !! 0) ++ "%")
    where
      battFile = "/sys/class/power_supply/BAT0/capacity"

-- TODO: Need to find out how to do this.
{-
getBatteryTime :: IO String
getBatteryTime = undefined
-}

getBatteryStatus :: IO String
getBatteryStatus = (runResourceT $ sourceFile battFile $= lines $$ consume) >>=
  (\t -> case (unpack $ t !! 0) of
    "Full"     -> return "(=)"
    "Charging" -> return "(+)"
    _          -> return "(-)"
  )
    where
      battFile = "/sys/class/power_supply/BAT0/status"
