module AoC2021Day4 (parseLines) where

import Data.List.Split
import Data.String

data Bingo = Bingo {draws :: [Int], boards :: [Board]}

type Board = [Row]

type Row = [Choice]

data Choice = Choice {value :: Int, marked :: Bool}

parseLines :: [String] -> Bingo
parseLines ss = Bingo {draws = draws, boards = boards}
  where
    draws = map read (splitOn "," (head ss))
    boards = map (map parseRow . take 5) (chunksOf 6 (drop 2 ss))

parseRow :: String -> Row
parseRow r = map newChoice (words r)

newChoice :: String -> Choice
newChoice c = Choice {value = read c, marked = False}

instance Show Bingo where
  show Bingo {draws = d, boards = bs} = show d ++ "\n\n" ++ concatMap (\b -> showBoard b ++ "\n") bs

showBoard :: Board -> String
showBoard = concatMap (\r -> showRow r ++ "\n")

showRow :: Row -> String
showRow = concatMap (\c -> show c ++ " ")

instance Show Choice where
  show Choice {value = v, marked = m} = marked ++ show v ++ pad
    where
      pad = if v < 10 then "  " else " "
      marked = if m then "X" else " "