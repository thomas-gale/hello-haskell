module AoCUtil (getLines, getInts) where

import Data.String

getLines :: FilePath -> IO [String]
getLines = fmap lines . readFile

getInts :: FilePath -> IO [Int]
getInts = fmap (map read . lines) . readFile
