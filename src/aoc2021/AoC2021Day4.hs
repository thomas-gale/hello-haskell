module AoC2021Day4 (part1, part2) where

import Data.List
import Data.List.Split
import Data.String

-- Types
data Bingo = Bingo {draws :: [Int], boards :: [Board], bingoBoards :: [Board]}

data Board = Board {rows :: [Row], bingoNum :: Maybe Int}
  deriving (Eq)

type Row = [Choice]

data Choice = Choice {value :: Int, marked :: Bool}
  deriving (Eq)

-- Logic
part1 :: [String] -> Int
part1 ss = score
  where
    initialBingo = parseLinesToBingo ss
    Bingo {bingoBoards = bbs} = playBingo initialBingo
    score = computeBoardScore $ head bbs

part2 :: [String] -> Int
part2 ss = score
  where
    initialBingo = parseLinesToBingo ss
    allBoards = playAllBingoBoards initialBingo
    score = computeBoardScore $ last allBoards

computeBoardScore :: Board -> Int
computeBoardScore Board {bingoNum = Nothing} = error "Can't score a board that isn't Bingo"
computeBoardScore Board {rows = rs, bingoNum = (Just bn)} = bn * unmarkedSum
  where
    unmarkedSum = foldl (\acc Choice {value = v, marked = m} -> if not m then acc + v else acc) 0 (concat rs)

-- Plays bingo over all boards, create a list of boards as they win.
playAllBingoBoards :: Bingo -> [Board]
playAllBingoBoards Bingo {draws = []} = []
playAllBingoBoards Bingo {boards = []} = []
playAllBingoBoards Bingo {draws = ds, boards = bs} = bbs ++ playAllBingoBoards Bingo {draws = remainDs, boards = filteredBs, bingoBoards = []}
  where
    Bingo {draws = remainDs, boards = remainBs, bingoBoards = bbs} = playBingo Bingo {draws = ds, boards = bs, bingoBoards = []}
    filteredBs = filter (`notElem` bbs) remainBs

playBingo :: Bingo -> Bingo
playBingo Bingo {draws = ds, boards = bs, bingoBoards = bbs} | bbs /= [] = Bingo {draws = ds, boards = bs, bingoBoards = bbs} -- Check for bingo boards (base case)
playBingo Bingo {draws = ds, boards = bs, bingoBoards = []} = playBingo Bingo {draws = tail ds, boards = newBoards, bingoBoards = bingoBoards} -- Play normal step
  where
    draw = head ds
    newBoards = map (markBoard draw) bs
    bingoBoards =
      filter
        ( \Board {bingoNum = bn} -> case bn of
            (Just _) -> True
            Nothing -> False
        )
        newBoards
playBingo _ = error "Unknown bingo case (empty draws/boards)"

markBoard :: Int -> Board -> Board
markBoard _ Board {rows = rows, bingoNum = (Just n)} = Board {rows = rows, bingoNum = Just n}
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

-- Helpers (parse inputs and pretty print boards)
parseLinesToBingo :: [String] -> Bingo
parseLinesToBingo ss = Bingo {draws = draws, boards = boards, bingoBoards = []}
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
  show Choice {value = v, marked = m} = pad ++ show v ++ marked
    where
      pad = if v < 10 then "  " else " "
      marked = if m then "X" else " "