module AoC2021TestDay4 (run) where

import AoC2021Day4
import AoCUtil

run = do
  lines <- getLines "./test/aoc2021/Day4.in"
  print $ part1 lines
  print $ part2 lines
