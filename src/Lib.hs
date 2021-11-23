module Lib
    ( hello 
    , doubleMe
    , doubleUs
    ) where

-- hello world
hello :: IO ()
hello = putStrLn "Hello Haskell"

-- baby steps
doubleMe x = x + x
doubleUs x y = x*2 + y*2
