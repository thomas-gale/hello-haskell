module AoC2021TestDay11 (run) where

import AoC2021Day11
import AoCUtil

run = do
  ss <- getLines "./test/aoc2021/Day11.in"
  print $ part1 ss
  print $ part2 ss
