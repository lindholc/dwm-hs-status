module Status.Widget.CPU (getCpuSpeed, getCpuUsage) where

import Control.Applicative
import Control.Monad (liftM)
import System.Process (readProcess)
import Text.Parsec

getCpuSpeed :: Int -> IO String
getCpuSpeed p = getCpuInfo >>= \x ->
  return $! either (const "?") id (parse speedParser "" (x !! 11))
  where
    getCpuInfo :: IO [String]
    getCpuInfo = liftM lines (readProcess "cpupower" ["frequency-info", "c" ++ show p] [])


getCpuUsage :: Int -> IO String
getCpuUsage = undefined

-- TODO: Type
speedParser = spaces >> string "current CPU frequency is " *> decimal
  where
    decimal = do
      start <- many1 digit
      char '.'
      end <- many1 digit
      return $ start ++ "." ++ end
