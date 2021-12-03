module AoC2015TestDay1 (run) where

import AoC2015Day1
import AoCUtil

run :: IO ()
run = do
  line <- readFile "./test/aoc2015/Day1.in"
  print $ part1 line
  print $ part2 line
