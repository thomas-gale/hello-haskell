module AoC2021TestDay9 (run) where

import AoC2021Day9
import AoCUtil

run = do
  ss <- getLines "./test/aoc2021/Day9.in"
  print $ part1 ss
  print $ part2 ss
