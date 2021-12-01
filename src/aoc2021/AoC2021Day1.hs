module AoC2021Day1 (numIncreases, threeMeasureSlidingWindowSum) where

-- Part 1
numIncreases :: [Int] -> Int
numIncreases ds = snd (foldl computeIncrease (Nothing, 0) ds)

computeIncrease :: (Maybe Int, Int) -> Int -> (Maybe Int, Int)
computeIncrease (Nothing, countIncrease) currDepth = (Just currDepth, 0)
computeIncrease (Just prevDepth, countIncrease) currDepth = (Just currDepth, countIncrease + increase)
  where
    increase
      | currDepth > prevDepth = 1
      | otherwise = 0

-- Part 2
threeMeasureSlidingWindowSum :: [Int] -> [Int]
threeMeasureSlidingWindowSum [] = []
threeMeasureSlidingWindowSum [_] = []
threeMeasureSlidingWindowSum [_, _] = []
threeMeasureSlidingWindowSum x =
  let win = take 3 x
   in sum win : threeMeasureSlidingWindowSum (tail x)
