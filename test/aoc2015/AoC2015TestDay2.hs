module AoC2015TestDay2 (run) where

import AoC2015Day2
import AoCUtil

run :: IO ()
run = do
  lines <- getLines "./test/aoc2015/Day2.in"
  print $ part1 lines
  print $ part2 lines
