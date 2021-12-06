module AoC2021TestDay6 (run) where

import AoC2021Day6
import AoCUtil

run = do
  xs <- getCsvInts "./test/aoc2021/Day6.in"
  print $ part1 xs
  print $ part2 xs
