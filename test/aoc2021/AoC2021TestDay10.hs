module AoC2021TestDay10 (run) where

import AoC2021Day10
import AoCUtil

run = do
  ss <- getLines "./test/aoc2021/Day10.in"
  print $ part1 ss
  print $ part2 ss
