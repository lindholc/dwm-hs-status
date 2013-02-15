module Status.Audio (getMuteStatus, getVolLevel) where

import Control.Applicative hiding (many)
import Control.Monad (liftM)
import System.Process (readProcess)
import Text.Parsec
import Prelude hiding (getLine)

-- TODO: Clean up the parser
getMuteStatus :: IO String
getMuteStatus = getAudioInfo >>= \x ->
  return $ either show id (parse muteParser "" x)
  where
    muteParser = spaces >> string "Mono: Playback " >> num >> space >> bval >> space >> bval >> space *> bval

-- TODO: Clean up the parser
getVolLevel :: IO String
getVolLevel = getAudioInfo >>= \x ->
  return $ either show id (parse volParser "" x)
  where
    volParser = spaces >> string "Mono: Playback " >> num >> space *> bval

-- TODO: Is there a more specialized amixer command to just get the info
-- for the Master?
getAudioInfo :: IO String
getAudioInfo = liftM getLine (readProcess "amixer" [] [])
  where
    getLine :: String -> String
    getLine = (!! 4) . lines

-- TODO: Type signature.
bval = string "[" *> manyTill anyChar (try (string "]"))

-- TODO: Type signature
num = skipMany digit
