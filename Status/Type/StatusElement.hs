module Status.Type.StatusElement (StatusElement(..)) where

data StatusElement = Flag String
                   | Str String
                   deriving (Eq)
