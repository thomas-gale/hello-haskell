module AoC2021Day5 (part1, part2) where

import qualified Data.List.Split as S
import qualified Data.Map as M

data Vec2 = Vec2 {x :: Int, y :: Int}
  deriving (Eq, Ord, Show)

data Line = Line {start :: Vec2, end :: Vec2}
  deriving (Show)

-- map locations to vent overlap count
type Field = M.Map Vec2 Int

part1 :: [String] -> Int
part1 ss = score finalField
  where
    lines = map parseLine ss
    ortholines = filter orthoLine lines
    finalField = foldl addLineToField M.empty ortholines

part2 :: [String] -> Int
part2 ss = score finalField
  where
    lines = map parseLine ss
    finalField = foldl addLineToField M.empty lines

score :: Field -> Int
score = M.foldl (\score curr -> if curr >= 2 then score + 1 else score) 0

addLineToField :: Field -> Line -> Field
addLineToField f ol = newField
  where
    newVecs = lineToVecs ol
    newField = foldl (\f v -> M.insertWith (+) v 1 f) f newVecs

lineToVecs :: Line -> [Vec2]
lineToVecs Line {start = Vec2 {x = sx, y = sy}, end = Vec2 {x = ex, y = ey}} | sx == ex && sy == ey = [Vec2 {x = sx, y = sy}] -- base
lineToVecs Line {start = Vec2 {x = sx, y = sy}, end = Vec2 {x = ex, y = ey}} | sy == ey = Vec2 {x = min sx ex, y = sy} : lineToVecs (Line {start = Vec2 {x = min sx ex + 1, y = sy}, end = Vec2 {x = max sx ex, y = ey}}) -- horizontal
lineToVecs Line {start = Vec2 {x = sx, y = sy}, end = Vec2 {x = ex, y = ey}} | sx == ex = Vec2 {x = sx, y = min sy ey} : lineToVecs (Line {start = Vec2 {x = sx, y = min sy ey + 1}, end = Vec2 {x = ex, y = max sy ey}}) -- vertical
lineToVecs Line {start = Vec2 {x = sx, y = sy}, end = Vec2 {x = ex, y = ey}} = Vec2 {x = sx, y = sy} : lineToVecs (Line {start = start', end = Vec2 {x = ex, y = ey}}) -- else diagonal
  where
    start' = Vec2 {x = if ex - sx > 0 then sx + 1 else sx -1, y = if ey - sy > 0 then sy + 1 else sy -1}

orthoLine :: Line -> Bool
orthoLine l = x (start l) == x (end l) || y (start l) == y (end l)

parseLine :: String -> Line
parseLine s = Line {start = parseVec2 (head splitLine), end = parseVec2 (last splitLine)}
  where
    splitLine = S.splitOn " " s

parseVec2 :: String -> Vec2
parseVec2 s = Vec2 {x = read (head vals), y = read (last vals)}
  where
    vals = S.splitOn "," s
