module AoC2021TestDay1 (run) where

import AoC2021Day1
import AoCUtil

run :: IO ()
run = do
  -- Part 1
  ints <- getInts "./test/aoc2021/Day1.in"
  let res1 = numIncreases ints
  print res1

  -- Part 2
  let res2 = numIncreases $ threeMeasureSlidingWindowSum ints
  print res2
