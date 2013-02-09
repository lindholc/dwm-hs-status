module Status.Wireless (getStrength) where

import Control.Applicative
import System.Process (readProcess)
import Text.Parsec

getStrength :: IO String
getStrength = input >>= \x ->
  return $! either (const "?") id (parse wifiParser "" x)
    where
      -- Read the string, break it into lines, select the correct line.
      input :: IO String
      input = readProcess "iwconfig" ["wlan0"] [] >>= \x ->
        return $ lines x !! 5

-- TODO: Type signature?
wifiParser = spaces >> string "Link Quality=" *> quality
  where
    quality = many1 digit
