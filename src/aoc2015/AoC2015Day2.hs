module AoC2015Day2 (part1, part2) where

import Data.List
import Data.List.Split

type Length = Int

type Width = Int

type Height = Int

type Area = Int

parseInput :: [String] -> [(Length, Width, Height)]
parseInput = map (\s -> let [l, w, h] = splitOn "x" s in (read l, read w, read h))

area :: (Length, Width, Height) -> Area
area (l, w, h) = 2 * s1 + 2 * s2 + 2 * s3 + slack
  where
    s1 = l * w
    s2 = w * h
    s3 = l * h
    slack = minimum [s1, s2, s3]

part1 :: [String] -> Area
part1 ss = sum (map area (parseInput ss))

ribbon :: (Length, Width, Height) -> Length
ribbon (l, w, h) = 2 * sml1 + 2 * sml2 + l * w * h
  where
    (sml1 : sml2 : _) = sort [l, w, h]

part2 :: [String] -> Length
part2 ss = sum (map ribbon (parseInput ss))