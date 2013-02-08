{-# LANGUAGE BangPatterns #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever, void, liftM)
import System.Cmd

import Status.Battery
import Status.Time
import Status.Type.Action
import Status.Type.StatusElement
import Status.Util

main :: IO ()
main = do
  m <- mapM startAction actions
  forever $ do
    status <- makeStatus statusDef m
    putStatus status
    threadDelay (seconds 1)

statusDef :: [StatusElement]
statusDef = [Flag "batc", space, Flag "bats", bar, Flag "time"]

actions :: [Action]
actions = [ Action (Flag "time") (seconds 1) getTime
          , Action (Flag "batc") (seconds 10) getBatCapacity
          , Action (Flag "bats") (seconds 10) getBatStatus]

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
    makeStatus' (Sep t) = return t
    makeStatus' f = maybe (return "?") readTVarIO (lookup f m)

putStatus :: String -> IO ()
putStatus status = void $ rawSystem "xsetroot" ["-name", status]
