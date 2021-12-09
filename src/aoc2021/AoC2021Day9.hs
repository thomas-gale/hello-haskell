module AoC2021Day9 (part1, part2) where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as S
import qualified Data.Map as M
import qualified Data.Set as Set

type Vec2 = (Int, Int)

type HeightMap = M.Map Vec2 Int

type FillSet = Set.Set Vec2

part1 :: [String] -> Int
part1 ss = minimaSum
  where
    hm = parseHeightMap ss
    minima = M.foldrWithKey (\p h min -> let ns = getNeighbours p hm in if isMinima h ns then (p, h) : min else min) [] hm
    minimaSum = foldr (\(p, h) sum -> sum + h + 1) 0 minima --risk level

part2 :: [String] -> Int
part2 ss = top3Product
  where
    hm = parseHeightMap ss
    minima = M.foldrWithKey (\p h min -> let ns = getNeighbours p hm in if isMinima h ns then (p, h) : min else min) [] hm
    fillSets = foldl (\fs (pmin, _) -> floodFill pmin hm Set.empty : fs) [] minima
    fillSetSizes = L.reverse $ L.sort $ map Set.size fillSets
    top3Product = product (take 3 fillSetSizes)

floodFill :: Vec2 -> HeightMap -> FillSet -> FillSet
floodFill p hm fs | M.findWithDefault 9 p hm == 9 = fs -- boundary point
floodFill p hm fs | p `Set.member` fs = fs -- already filled point
floodFill p hm fs = fs''
  where
    fs' = Set.insert p fs -- set the node
    ns = getNeighbours p hm -- reuse the neighbour code from part 1
    fs'' = foldl (\fs' (np, _) -> floodFill np hm fs') fs' ns -- multiway recursion

isMinima :: Int -> [(Vec2, Int)] -> Bool
isMinima h = all (\(_, nh) -> h < nh)

getNeighbours :: Vec2 -> HeightMap -> [(Vec2, Int)]
getNeighbours (px, py) hm = ns
  where
    adj = [(-1, 0), (0, -1), (1, 0), (0, 1)]
    ns = foldl (\acc (ax, ay) -> let p = (px + ax, py + ay) in appendIfExist p hm acc) [] adj

appendIfExist :: Vec2 -> HeightMap -> [(Vec2, Int)] -> [(Vec2, Int)]
appendIfExist p hm hs = hs'
  where
    x = M.lookup p hm
    hs' = case x of
      Nothing -> hs
      Just i -> (p, i) : hs

parseHeightMap :: [String] -> HeightMap
parseHeightMap ss = foldl (\hm (y, sxs) -> parseLine sxs y hm) hm (zip [0 ..] ss)
  where
    hm = M.empty :: HeightMap

-- String of x positions (line), current y position, current heightmap
parseLine :: String -> Int -> HeightMap -> HeightMap
parseLine sxs y hm = foldl (\hm (x, h) -> M.insert (x, y) h hm) hm (zip [0 ..] hs)
  where
    hs = map C.digitToInt sxs
