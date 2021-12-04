module AoC2021TestDay4 (run) where

import AoC2021Day4
import AoCUtil

run :: IO ()
run = do
  lines <- getLines "./test/aoc2021/Day4.s.in"
  print $ parseLines lines
