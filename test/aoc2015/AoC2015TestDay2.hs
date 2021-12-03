module AoC2015TestDay2 (run) where

import AoC2015Day2
import AoCUtil

run :: IO ()
run = do
  line <- readFile "./test/aoc2015/Day2.s.in"
  print line
  -- print $ part1 line
  -- print $ part2 line
