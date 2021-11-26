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
    rows,
    cols,
    boxs,
    nodups,
    Choices,
    choices,
    cp,
    collapse,
  )
where

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

collapse :: Matrix [a] -> [Matrix a]
collapse = cp . map cp
