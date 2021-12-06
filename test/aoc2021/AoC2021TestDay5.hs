module AoC2021TestDay5 (run) where

import AoC2021Day5
import AoCUtil

run = do
  ss <- getLines "./test/aoc2021/Day5.in"
  print $ part1 ss
  print $ part2 ss
