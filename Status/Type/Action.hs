module Status.Type.Action (Action(..)) where

import Status.Type.StatusElement

-- TODO: Can the StatusElement be restrictied to Flag?
data Action = Action StatusElement Int (IO String)
