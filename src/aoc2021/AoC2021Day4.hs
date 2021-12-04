module AoC2021Day4 (parseLinesToBingo, playBingo, part1) where

import Data.List
import Data.List.Split
import Data.String

-- Types
data Bingo = Bingo {draws :: [Int], boards :: [Board], bingoBoard :: Maybe Board}

data Board = Board {rows :: [Row], bingoNum :: Maybe Int}

type Row = [Choice]

data Choice = Choice {value :: Int, marked :: Bool}

-- Logic
part1 :: [String] -> Int
part1 ss = res
  where
    initialBingo = parseLinesToBingo ss
    Bingo {bingoBoard = bb} = playBingo initialBingo
    res = case bb of
      (Just winb) -> computeBoardScore winb
      Nothing -> -1

computeBoardScore :: Board -> Int
computeBoardScore Board {bingoNum = Nothing} = error "Can't score a board that isn't Bingo"
computeBoardScore Board {rows = rs, bingoNum = (Just bn)} = bn * unmarkedSum
  where
    unmarkedSum = foldl (\acc Choice {value = v, marked = m} -> if not m then acc + v else acc) 0 (concat rs)

playBingo :: Bingo -> Bingo
playBingo Bingo {boards = []} = error "No boards!"
playBingo Bingo {draws = []} = error "Out of draws! (no bingo :()"
playBingo Bingo {draws = ds, boards = bs, bingoBoard = (Just b)} = Bingo {draws = ds, boards = bs, bingoBoard = Just b} -- Check for Bingo!
playBingo Bingo {draws = ds, boards = bs} = playBingo Bingo {draws = tail ds, boards = newBoards, bingoBoard = bingoBoard} -- Play normal step
  where
    draw = head ds
    newBoards = map (markBoard draw) bs
    bingoBoard =
      find
        ( \Board {bingoNum = bn} -> case bn of
            (Just _) -> True
            Nothing -> False
        )
        newBoards

markBoard :: Int -> Board -> Board
markBoard _ Board {bingoNum = (Just _)} = error "Can't mark board, alreay Bingo!"
markBoard draw Board {rows = rows, bingoNum = Nothing} = Board {rows = newRows, bingoNum = if isBingo then Just draw else Nothing}
  where
    newRows = map (markRow draw) rows
    isBingo = checkBoardComplete (Board {rows = newRows, bingoNum = Nothing})

markRow :: Int -> Row -> Row
markRow draw = map (\Choice {value = v, marked = m} -> Choice {value = v, marked = (draw == v) || m})

checkBoardComplete :: Board -> Bool
checkBoardComplete b = anyRowsComplete || anyColsComplete
  where
    anyRowsComplete = any checkRowComplete $ rows b
    anyColsComplete = any checkRowComplete $ cols b

checkRowComplete :: Row -> Bool
checkRowComplete = all (\Choice {marked = m} -> m)

cols :: Board -> [Row]
cols Board {rows = r} = transpose r

-- Helpers
parseLinesToBingo :: [String] -> Bingo
parseLinesToBingo ss = Bingo {draws = draws, boards = boards, bingoBoard = Nothing}
  where
    draws = map read (splitOn "," (head ss))
    boards = map (\b -> Board {rows = map parseRow (take 5 b), bingoNum = Nothing}) (chunksOf 6 (drop 2 ss))

parseRow :: String -> Row
parseRow r = map newChoice (words r)

newChoice :: String -> Choice
newChoice c = Choice {value = read c, marked = False}

instance Show Bingo where
  show Bingo {draws = d, boards = bs} = show d ++ "\n\n" ++ concatMap (\b -> showBoard b ++ "\n\n") bs

showBoard :: Board -> String
showBoard Board {rows = rs, bingoNum = bn} = concatMap (\r -> showRow r ++ "\n") rs ++ "Is Bingo: " ++ show bn ++ "\n"

showRow :: Row -> String
showRow = concatMap (\c -> show c ++ " ")

instance Show Choice where
  show Choice {value = v, marked = m} = marked ++ show v ++ pad
    where
      pad = if v < 10 then "  " else " "
      marked = if m then "X" else " "