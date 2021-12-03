module AoC2021TestDay3 (run) where

import AoC2021Day3
import AoCUtil

run :: IO ()
run = do
  lines <- getLines "./test/aoc2021/Day3.in"
  print $ part1 lines
  print $ part2 lines
