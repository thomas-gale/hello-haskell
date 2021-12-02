module AoC2021Day2 (part2) where

import Data.List

-- Part 1 merged into 2
type Horizontal = Int

type Vertical = Int

type Aim = Int

data Command
  = Forward Horizontal
  | Down Vertical
  | Up Vertical
  | NoOp
  deriving (Show)

parseToCommands :: [String] -> [Command]
parseToCommands = map parseToCommand

parseToCommand :: String -> Command
parseToCommand s | Just val <- stripPrefix "forward" s = Forward (read val)
parseToCommand s | Just val <- stripPrefix "down" s = Down (read val)
parseToCommand s | Just val <- stripPrefix "up" s = Up (read val)
parseToCommand _ = NoOp

type SubState = (Horizontal, Vertical, Aim)

moveSub :: SubState -> Command -> SubState
moveSub (h, v, a) (Forward x) = (h + x, v + a * x, a)
moveSub (h, v, a) (Down x) = (h, v, a + x)
moveSub (h, v, a) (Up x) = (h, v, a - x)
moveSub (h, v, a) _ = (h, v, a)

commandListMoveSub :: SubState -> [Command] -> SubState
commandListMoveSub = foldl moveSub

part2 :: [String] -> Int
part2 cs = let (finalH, finalV, _) = commandListMoveSub (0, 0, 0) (parseToCommands cs) in (finalH * finalV)
