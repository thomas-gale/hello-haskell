module AoC2021Day6 (part1, part2) where

type Days = Int

type BreedCycle = Int

type NumberFish = Integer -- We need a big integer as numbers get very large

type ShoalState = [(BreedCycle, NumberFish)]

part1 :: [BreedCycle] -> Integer
part1 = computeNumberFish 80

part2 :: [BreedCycle] -> Integer
part2 = computeNumberFish 256

computeNumberFish :: Days -> [BreedCycle] -> Integer
computeNumberFish ds fcs = foldl (\acc (_, n) -> acc + n) 0 finalState
  where
    finalState = simulate ds (genShoalState fcs emptyShoalState)

simulate :: Days -> ShoalState -> ShoalState
simulate 0 ss = ss
simulate ds ss = simulate (ds -1) (step ss)

step :: ShoalState -> ShoalState
step ss = ss''
  where
    breedNo = snd $ head (filter (\(i, _) -> i == 0) ss) -- determine breeders
    ss' = foldl (\acc (si, sn) -> map (\(ai, an) -> if ai == si -1 then (ai, sn) else (ai, an)) acc) emptyShoalState ss -- tick down
    ss'' = map (\(si, sn) -> if si == 6 || si == 8 then (si, sn + breedNo) else (si, sn)) ss' -- spawn new offspring and reset breeders

genShoalState :: [BreedCycle] -> ShoalState -> ShoalState
genShoalState [] ss = ss
genShoalState (fc : fcs) ss = genShoalState fcs ss'
  where
    ss' = map (\(i, n) -> if i == fc then (i, n + 1) else (i, n)) ss

emptyShoalState :: ShoalState
emptyShoalState = zip [0 ..] (replicate 9 0)
