-- Based on http://www.cs.nott.ac.uk/~pszgmh/sudoku.lhs and the 3 part lecture series (on YouTube).
module Sudoku
  ( Value,
    Row,
    Matrix,
    Grid,
    boxsize,
    values,
    empty,
    single,
    easy,
    gentle,
    diabolical,
    unsolvable,
    minimal,
    blank,
    rows,
    cols,
    boxs,
    nodups,
    valid,
    Choices,
    choices,
    cp,
    collapse,
    solve,
    minus,
    reduce,
    prune,
    solve2,
    fix,
    solve3,
    complete,
    void,
    consistent,
    safe,
    blocked,
    solve4,
    search,
    expand,
  )
where

import qualified Data.Functor as Notes
import Data.List

-- Basic declarations
type Value = Char

type Row a = [a]

type Matrix a = [Row a]

type Grid = Matrix Value

-- Basic definitions
boxsize :: Int
boxsize = 3

values :: [Value]
values = ['1' .. '9']

empty :: Value -> Bool
empty = (== '.')

single :: [a] -> Bool
single [_] = True
single _ = False

-- Test cases
easy :: Grid
easy =
  [ "2....1.38",
    "........5",
    ".7...6...",
    ".......13",
    ".981..257",
    "31....8..",
    "9..8...2.",
    ".5..69784",
    "4..25...."
  ]

gentle :: Grid
gentle =
  [ ".1.42...5",
    "..2.71.39",
    ".......4.",
    "2.71....6",
    "....4....",
    "6....74.3",
    ".7.......",
    "12.73.5..",
    "3...82.7."
  ]

diabolical :: Grid
diabolical =
  [ ".9.7..86.",
    ".31..5.2.",
    "8.6......",
    "..7.5...6",
    "...3.7...",
    "5...1.7..",
    "......1.9",
    ".2.6..35.",
    ".54..8.7."
  ]

unsolvable :: Grid
unsolvable =
  [ "1..9.7..3",
    ".8.....7.",
    "..9...6..",
    "..72.94..",
    "41.....95",
    "..85.43..",
    "..3...7..",
    ".5.....4.",
    "2..8.6..9"
  ]

minimal :: Grid
minimal =
  [ ".98......",
    "....7....",
    "....15...",
    "1........",
    "...2....9",
    "...9.6.82",
    ".......3.",
    "5.1......",
    "...4...2."
  ]

blank :: Grid
blank = replicate n (replicate n '.')
  where
    n = boxsize ^ 2

-- Extracting rows, columns and boxes.
rows :: Matrix a -> [Row a]
-- id is same as fn x = x
rows = id

cols :: Matrix a -> [Row a]
cols = transpose

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

boxs :: Matrix a -> [Row a]
boxs = unpack . map cols . pack
  where
    pack = split . map split
    split = chop boxsize
    unpack = map concat . concat

-- Validity checking
nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x : xs) = x `notElem` xs && nodups xs

valid :: Grid -> Bool
valid g =
  all nodups (rows g)
    && all nodups (cols g)
    && all nodups (boxs g)

-- Basic solver section
type Choices = [Value]

choices :: Grid -> Matrix Choices
choices = map (map choice)
  where
    choice v = if empty v then values else [v]

-- cartesian product of two lists
cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs : xss) = [y : ys | y <- xs, ys <- cp xss]

-- turn matrix of choices into a list of matrices
collapse :: Matrix [a] -> [Matrix a]
collapse = cp . map cp

solve :: Grid -> [Grid]
solve = filter valid . collapse . choices

-- pruning the search space
minus :: Choices -> Choices -> Choices
-- (\\) operator is the list difference (non associative) from Data.List package
-- [1,2,3] \\ [2,3] = [1]
xs `minus` ys = if single xs then xs else xs \\ ys

-- Remove any single values from the multiple other multiple choices in a row
-- reduce ["3", "1", "123"] -> ["3", "1", "2"]
reduce :: Row Choices -> Row Choices
reduce xss = [xs `minus` singles | xs <- xss]
  where
    singles = concat (filter single xss)

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows
  where
    -- The f . something . f is the nice property that rows/cols/boxs have which is that rows . rows = id
    pruneBy f = f . map reduce . f

-- solve 2 (second iteration) - still not infeasible
solve2 :: Grid -> [Grid]
solve2 = filter valid . collapse . prune . choices

-- repeated pruning
fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x'
  where
    x' = f x

-- solve 3 - works for each, still too many choices for harder puzzles
solve3 :: Grid -> [Grid]
solve3 = filter valid . collapse . fix prune . choices

-- further properties to achieve final solution algorithm
complete :: Matrix Choices -> Bool
complete = all (all single)

void :: Matrix Choices -> Bool
void = any (any null)

-- check that row does not contain more that one same value of a single choice (otherwise sudoku is not solvable)
consistent :: Row Choices -> Bool
consistent = nodups . concat . filter single

-- safe runs consistent over all rows/cols/boxs
safe :: Matrix Choices -> Bool
safe mc = all consistent (rows mc) && all consistent (cols mc) && all consistent (boxs mc)

-- put checks together, matrix is blocked if void or not safe
blocked :: Matrix Choices -> Bool
blocked m = void m || not (safe m)

-- Putting final solve together
solve4 :: Grid -> [Grid]
solve4 = search . prune . choices

search :: Matrix Choices -> [Grid]
search m
  | blocked m = []
  | complete m = collapse m
  | otherwise = [g | m' <- expand m, g <- search (prune m')]

-- Behaves the same as collapse except only collapses the first square with more than one choice.
-- Notes.
-- List span: applied to a predicate p and a list xs, returns a tuple where first element is longest prefix
-- (possibly empty) of xs of elements that satisfy p and second element is the remainder of the list
expand :: Matrix Choices -> [Matrix Choices]
expand m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
  where
    (rows1, row : rows2) = span (all single) m
    (row1, cs : row2) = span single row
