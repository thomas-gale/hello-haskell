module AoC2021Day11 (part1, part2) where

import Data.Char (digitToInt)
import qualified Data.Map as M

type Energy = Int

type Vec2 = (Int, Int)

-- energy level, has triggered flash
type OctoState = (Int, Bool)

type Grid = M.Map Vec2 OctoState

part1 :: [String] -> Int
part1 ss = nf
  where
    grid = parseToGrid ss
    (_, nf) = foldr (\_ (accG, accNf) -> let (g', ns) = step accG in (g', accNf + ns)) (grid, 0) [1 .. 100]

part2 :: [String] -> Int
part2 ss = ns
  where
    grid = parseToGrid ss
    (_, ns) = stepTillSyncFlash grid 1

-- returns new grid and number of steps (start at step 1)
stepTillSyncFlash :: Grid -> Int -> (Grid, Int)
stepTillSyncFlash grid steps | let (grid', flashes) = step grid in (flashes == M.size grid) = (grid, steps)
stepTillSyncFlash grid steps = let (grid', _) = step grid in stepTillSyncFlash grid' (steps + 1)

-- return new grid and number of flashes
step :: Grid -> (Grid, Int)
step g = (g''', nflash)
  where
    g' = M.map (\(s, f) -> (s + 1, f)) g -- increase each octo energy level
    g'' = M.foldrWithKey (\v _ acc -> flash acc v) g' g' -- flash updates
    nflash = M.foldr (\(_, f) acc -> if f then acc + 1 else acc) 0 g'' -- count the flashes
    g''' = M.map (\(s, f) -> if f then (0, False) else (s, f)) g'' -- reset energy levels

flash :: Grid -> Vec2 -> Grid
flash g v | let (e, f) = M.findWithDefault (-1, False) v g in (e > 9 && not f) = foldl flash g'' ns
  where
    (e, f) = M.findWithDefault (-1, False) v g
    g' = M.insert v (e, True) g -- set current node as flashed
    ns = neighbours v g' -- get neigbours
    g'' = foldr (\n acc -> let (ne, nf) = M.findWithDefault (-1, False) n acc in M.insert n (ne + 1, nf) acc) g' ns -- increase neighbour energy level
flash g _ = g -- non flash case

neighbours :: Vec2 -> Grid -> [Vec2]
neighbours (px, py) grid = ns
  where
    adj = [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1]]
    ns = foldl (\acc (ax, ay) -> let p = (px + ax, py + ay) in appendIfExist p grid acc) [] adj

appendIfExist :: Vec2 -> Grid -> [Vec2] -> [Vec2]
appendIfExist p grid ps = ps'
  where
    x = M.lookup p grid
    ps' = case x of
      Nothing -> ps
      Just _ -> p : ps

parseToGrid :: [String] -> Grid
parseToGrid ss = grid
  where
    lineInt = map (map digitToInt) ss
    grid = foldr (\(y, li) m -> parseLineToGrid y li m) M.empty (zip [0 ..] lineInt)

parseLineToGrid :: Int -> [Int] -> Grid -> Grid
parseLineToGrid y s g = foldr (\(x, e) m -> M.insert (x, y) (e, False) m) g (zip [0 ..] s)

renderGrid :: Grid -> String
renderGrid grid = foldl (\ls y -> ls ++ "\n" ++ renderLine y (minX, maxX) grid) [] [minY .. maxY]
  where
    (xs, ys) = unzip (M.keys grid)
    (minX, maxX) = (minimum xs, maximum xs)
    (minY, maxY) = (minimum ys, maximum ys)

renderLine :: Int -> (Int, Int) -> Grid -> String
renderLine y (xmin, xmax) grid = foldl (\l x -> let (pe, pf) = M.findWithDefault (-1, False) (x, y) grid in l ++ " " ++ show pe) "" [xmin .. xmax]
