module AoC2015Day1 (part1, part2) where

-- Complete overkill with types and whatnot.
data Direction = Up | Down

type Level = Int

type Step = Int

parseToDirection :: Char -> Maybe Direction
parseToDirection '(' = Just Up
parseToDirection ')' = Just Down
parseToDirection _ = Nothing

parseToDirections :: String -> [Maybe Direction]
parseToDirections = map parseToDirection

assignEnteredBasement :: Maybe Step -> Step -> Level -> Maybe Step
assignEnteredBasement (Just s) _ _ = Just s -- Already entered the basement, don't override
assignEnteredBasement Nothing s l = if l == -1 then Just s else Nothing

-- Returns (Final Position, Current Step, First Entered Basement Step)
computeFloor :: [Maybe Direction] -> (Level, Step, Maybe Step)
computeFloor =
  foldl
    ( \(currLvl, currStep, maybeBasement) d -> case d of
        Just Up -> (currLvl + 1, currStep + 1, assignEnteredBasement maybeBasement currStep currLvl)
        Just Down -> (currLvl - 1, currStep + 1, assignEnteredBasement maybeBasement currStep currLvl)
        _ -> (currLvl, currStep + 1, maybeBasement)
    )
    (0, 0, Nothing)

getLevel :: (Level, Step, Maybe Step) -> Level
getLevel (f, _, _) = f

getBasementStep :: (Level, Step, Maybe Step) -> Step
getBasementStep (_, _, Nothing) = -1 -- Failed to enter the basement
getBasementStep (_, _, Just s) = s

part1 :: String -> Level
part1 = getLevel . computeFloor . parseToDirections

part2 :: String -> Int
part2 = getBasementStep . computeFloor . parseToDirections