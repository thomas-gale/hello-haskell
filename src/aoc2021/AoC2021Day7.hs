module AoC2021Day7 (part1, part2) where

import qualified Data.List as L

part1 :: [Int] -> Int
part1 xs = res
  where
    m = median xs
    res = sumDistance m xs

part2 :: [Int] -> Int
part2 xs = res
  where
    m = mean xs
    res = minimum (map (`sumTriangleDistance` xs) [m - 5, m - 4 .. m + 5]) --Search 5 points either side of mean.

sumDistance :: Int -> [Int] -> Int
sumDistance x = foldr (\curr acc -> acc + abs (curr - x)) 0

sumTriangleDistance :: Int -> [Int] -> Int
sumTriangleDistance x = foldr (\curr acc -> let d = abs (curr - x) in (acc + ((d * (d + 1)) `div` 2))) 0

median :: [Int] -> Int
median [] = error "Empty list"
median [x] = x
median xs = L.sort xs !! (length xs `div` 2)

mean :: [Int] -> Int
mean xs = sum xs `div` length xs
