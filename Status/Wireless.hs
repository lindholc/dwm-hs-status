module Status.Wireless (getStrength, getESSID) where

import Control.Applicative
import Control.Monad (liftM)
import System.Process (readProcess)
import Text.Parsec

getStrength :: IO String
getStrength = getWirelessInfo >>= \x ->
  return $! either (const "?") id (parse signalParser "" (x !! 5))

getESSID :: IO String
getESSID = getWirelessInfo >>= \x ->
  return $! either (const "?") id (parse essidParser "" (head x))

getWirelessInfo :: IO [String]
getWirelessInfo = liftM lines (readProcess "iwconfig" ["wlan0"] [])

-- TODO: Type signature?
signalParser = spaces >> string "Link Quality=" *> quality
  where
    quality = many1 digit

-- TODO: Type signature?
essidParser = manyTill anyChar (try (string "ESSID:\"")) *> essid
  where
    essid = manyTill anyChar (try (string "\""))
