module AoC2021Day3 (part1, part2) where

import Data.List

type Bit = Int

parseInput :: [String] -> [[Bit]]
parseInput = map (map (read . pure :: Char -> Bit))

-- Given a bit (0 or 1), sum the number of occurances in the list of [bit] input
countBits :: Bit -> [[Bit]] -> [Bit]
countBits bit = foldl (zipWith (\a b -> if b == bit then a + 1 else a)) (repeat (0 :: Bit))

-- mostCommon use op (>=) least common use op (<)
most :: (Bit -> Bit -> Bool) -> [[Bit]] -> [Bit]
most op bss = zipWith (\o z -> (if o `op` z then 1 else 0)) ones zeros
  where
    ones = countBits 1 bss
    zeros = countBits 0 bss

convertToDecimal :: [Bit] -> Int
convertToDecimal = foldl (\acc x -> acc * 2 + x) 0

part1 :: [String] -> Int
part1 ss = gamma * epsilon
  where
    input = parseInput ss
    gamma = convertToDecimal $ most (>=) input
    epsilon = convertToDecimal $ most (<) input

type Filter = [Bit]

filterBits :: Filter -> [Bit] -> Bool
filterBits filter bs = all (== True) (zipWith (==) filter bs)

-- op == (>) for most commmon and (<) for least common
-- start this recusive function with empty [] filter
rating :: (Bit -> Bit -> Bool) -> Filter -> [[Bit]] -> [[Bit]]
rating _ _ [] = []
rating _ _ [x] = [x]
rating op oldfilter bss = rating op newFilter filteredBss
  where
    newFilter = oldfilter ++ [most op bss !! length oldfilter]
    filteredBss = filter (filterBits newFilter) bss

oxygenGeneratorRating :: [[Bit]] -> Int
oxygenGeneratorRating = convertToDecimal . head . rating (>=) []

co2ScrubberRating :: [[Bit]] -> Int
co2ScrubberRating = convertToDecimal . head . rating (<) []

part2 :: [String] -> Int
part2 ss = oxygenGeneratorRating input * co2ScrubberRating input
  where
    input = parseInput ss
