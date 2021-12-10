module AoC2021Day10 (part1, part2) where

import Data.List (sort)
import Data.Maybe (mapMaybe)

type ChunkStack = String

part1 :: [String] -> Int
part1 ss = score
  where
    firstIllegalChars = mapMaybe (\s -> let (ic, _) = parseChunkStack s [] in ic) ss
    score = foldl (\score c -> score + syntaxScore c) 0 firstIllegalChars

part2 :: [String] -> Int
part2 ss = midScore
  where
    incompleteStacks = filter (not . null) (map (\s -> let (_, cs) = parseChunkStack s [] in cs) ss)
    autoCompletions = map (map chunkCloser) incompleteStacks
    completionScores = map (foldl (\scr c -> (scr * 5) + autoComplScore c) 0) autoCompletions
    sortedComplScores = sort completionScores
    midScore = sortedComplScores !! (length sortedComplScores `div` 2)

autoComplScore :: Char -> Int
autoComplScore ')' = 1
autoComplScore ']' = 2
autoComplScore '}' = 3
autoComplScore '>' = 4
autoComplScore _ = 0

syntaxScore :: Char -> Int
syntaxScore ')' = 3
syntaxScore ']' = 57
syntaxScore '}' = 1197
syntaxScore '>' = 25137
syntaxScore _ = 0

-- Return the first illegal character and the chunk stack
parseChunkStack :: String -> ChunkStack -> (Maybe Char, ChunkStack)
parseChunkStack [] [] = (Nothing, []) -- base
parseChunkStack (s : ss) cs | isChunkOpen s = parseChunkStack ss (s : cs) -- normal stack push
parseChunkStack (s : ss) (c : cs) | isChunkClose s && s == chunkCloser c = parseChunkStack ss cs -- normal stack pop
parseChunkStack (s : _) _ = (Just s, []) -- corrupted
parseChunkStack [] cs = (Nothing, cs) -- incomplete

isChunkOpen :: Char -> Bool
isChunkOpen '(' = True
isChunkOpen '[' = True
isChunkOpen '{' = True
isChunkOpen '<' = True
isChunkOpen _ = False

isChunkClose :: Char -> Bool
isChunkClose ')' = True
isChunkClose ']' = True
isChunkClose '}' = True
isChunkClose '>' = True
isChunkClose _ = False

chunkCloser :: Char -> Char
chunkCloser '(' = ')'
chunkCloser '[' = ']'
chunkCloser '{' = '}'
chunkCloser '<' = '>'
chunkCloser _ = error "unknown closer"
