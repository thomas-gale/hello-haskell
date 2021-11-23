module Lib
    ( hello 
    , doubleMe
    ) where

-- hello world
hello :: IO ()
hello = putStrLn "Hello Haskell"

-- baby steps
doubleMe x = x + x
