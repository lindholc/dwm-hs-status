module Status.Type.StatusElement (StatusElement(..)) where

data StatusElement = Flag String
                   | Sep String
                   deriving (Eq)
