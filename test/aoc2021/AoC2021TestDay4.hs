module AoC2021TestDay4 (run) where

import AoC2021Day4
import AoCUtil

run = do
  lines <- getLines "./test/aoc2021/Day4.s.in"
  let bingo = parseLinesToBingo lines
  print bingo
  print "Playing bingo... \n"
  print $ playBingo bingo
