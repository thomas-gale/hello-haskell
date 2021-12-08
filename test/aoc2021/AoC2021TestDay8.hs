module AoC2021TestDay8 (run) where

import AoC2021Day8
import AoCUtil

run = do
  ss <- getLines "./test/aoc2021/Day8.in"
  print $ part1 ss
  print $ part2 ss
