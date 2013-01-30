module Status.Util (strip, seconds) where

strip :: String -> String
strip = filter (/= '\n')

seconds :: Int -> Int
seconds = (* 1000000)
