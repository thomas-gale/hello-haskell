module AoC2021TestDay2 (run) where

import AoC2021Day2
import AoCUtil

run :: IO ()
run = do
  -- Part 2 (part 1 code got morphed)
  lines <- getLines "./test/aoc2021/Day2.in"
  print $ part2 lines
