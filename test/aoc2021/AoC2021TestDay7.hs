module AoC2021TestDay7 (run) where

import AoC2021Day7
import AoCUtil

run = do
  xs <- getCsvInts "./test/aoc2021/Day7.in"
  print $ part1 xs
  print $ part2 xs
