module AoCUtil (getInts) where

getInts :: FilePath -> IO [Int]
getInts = fmap (map read . lines) . readFile
