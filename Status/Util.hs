module Status.Util (strip, seconds, space, bar) where

import Status.Type.StatusElement

strip :: String -> String
strip = filter (/= '\n')

seconds :: Int -> Int
seconds = (* 1000000)

space :: StatusElement
space = Str " "

bar :: StatusElement
bar = Str " | "

