module AoCUtil (getLines, getInts, getCsvInts) where

import qualified Data.List.Split as S
import Data.String

getLines :: FilePath -> IO [String]
getLines = fmap lines . readFile

getInts :: FilePath -> IO [Int]
getInts = fmap (map read . lines) . readFile

getCsvInts :: FilePath -> IO [Int]
getCsvInts = fmap (map read . S.splitOn ",") . readFile
