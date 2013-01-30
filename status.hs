import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever, void, liftM)
import System.Cmd

import Status.Battery
import Status.Time
import Status.Util

data StatusElement = Flag String
                   | Sep String
                   deriving (Eq)

-- TODO: Can the StatusElement be restrictied to Flag?
data Action = Action StatusElement Int (IO String)

space :: StatusElement
space = Sep " "

bar :: StatusElement
bar = Sep " | "

-- TODO: Do all the threads die when the main thread dies?
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
      val <- a
      atomically $ writeTVar tvar val
      threadDelay t
  return (f, tvar)

-- TODO: Could I use concatMap instead of map?
makeStatus :: [StatusElement] -> [(StatusElement, TVar String)] -> IO String
makeStatus s m = liftM concat $ mapM makeStatus' s
  where
    makeStatus' :: StatusElement -> IO String
    makeStatus' (Sep t) = return t
    makeStatus' f = maybe (return "?") readTVarIO (lookup f m)

putStatus :: String -> IO ()
putStatus status = void $ rawSystem "xsetroot" ["-name", status]

