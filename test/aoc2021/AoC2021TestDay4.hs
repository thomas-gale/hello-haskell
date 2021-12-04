module AoC2021TestDay4 (run) where

import AoC2021Day4
import AoCUtil

run = do
  lines <- getLines "./test/aoc2021/Day4.in"
  -- let bingo = parseLinesToBingo lines
  -- print bingo
  -- print "Playing bingo..."
  -- print $ playBingo bingo
  -- print "Part 1"
  print $ part1 lines
