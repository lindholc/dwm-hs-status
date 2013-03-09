{-# LANGUAGE BangPatterns #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever, void, liftM)
import System.Cmd

import Status.Type.Action
import Status.Type.StatusElement
import Status.Util
import Status.Widget

-- This would be cool:
-- essid >s> wfs >|> batc >s> bats >|> time
statusDef :: [StatusElement]
statusDef = [Flag "time"]

actions :: [Action]
actions = [ Action (Flag "time") (seconds 1) getTime ]

main :: IO ()
main = startStatus actions statusDef

startStatus :: [Action] -> [StatusElement] -> IO ()
startStatus actions statusDef = do
  m <- mapM startAction actions
  forever $ do
    status <- makeStatus statusDef m
    putStatus status
    threadDelay (seconds 1)

startAction :: Action -> IO (StatusElement, TVar String)
startAction (Action f t a) = do
  tvar <- atomically $ newTVar ""
  forkIO $
    forever $ do
      -- TODO: Is it useful to make this strict?
      !val <- a
      atomically $ writeTVar tvar val
      threadDelay t
  return (f, tvar)

makeStatus :: [StatusElement] -> [(StatusElement, TVar String)] -> IO String
makeStatus s m = liftM concat $ mapM makeStatus' s
  where
    makeStatus' :: StatusElement -> IO String
    makeStatus' (Str s) = return s
    makeStatus' f = maybe (return "?") readTVarIO (lookup f m)

putStatus :: String -> IO ()
putStatus status = void $ rawSystem "xsetroot" ["-name", status]
