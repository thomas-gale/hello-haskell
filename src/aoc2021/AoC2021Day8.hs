module AoC2021Day8 (part1, part2) where

import qualified Data.List as L
import qualified Data.List.Split as S
import qualified Data.Map as M
import qualified Data.Set as Set

data Entry = Entry {signalPatterns :: [String], outputPattern :: [String]}
  deriving (Show)

part1 :: [String] -> Int
part1 ss = digitCounts
  where
    entries = parseEntries ss
    digitCounts = foldr (\e acc -> acc + countDigits [1, 4, 7, 8] (outputPattern e)) (0 :: Int) entries

part2 :: [String] -> Int
part2 ss = sum decodedEntries
  where
    entries = parseEntries ss
    decodedEntries = map decodeEntry entries

-- Part 2 stuff
decodeEntry :: Entry -> Int
decodeEntry e = res
  where
    sigSets = signalPatternSets (signalPatterns e)
    -- trivial decoding (like part 1)
    one = head (filter (\s -> Set.size s == 2) sigSets)
    four = head (filter (\s -> Set.size s == 4) sigSets)
    seven = head (filter (\s -> Set.size s == 3) sigSets)
    eight = head (filter (\s -> Set.size s == 7) sigSets)
    -- remaining numbers - sequence of filters and set logic using both the numbers we know and the segments we know (in their original position identity)
    zeroSixNine = filter (\s -> Set.size s == 6) sigSets
    twoThreeFive = filter (\s -> Set.size s == 5) sigSets
    a = seven `Set.difference` one
    bd = four `Set.difference` one
    eg = eight `Set.difference` (a `Set.union` four)
    zero = head (filter (\s -> Set.size (s `Set.intersection` bd) == 1) zeroSixNine)
    sixNine = filter (\s -> Set.size (s `Set.intersection` bd) == 2) zeroSixNine
    six = head (filter (\s -> Set.size (s `Set.intersection` eg) == 2) sixNine)
    nine = head (filter (\s -> Set.size (s `Set.intersection` eg) == 1) sixNine)
    b = zero `Set.difference` ((a `Set.union` one) `Set.union` eg)
    five = head (filter (\s -> Set.size (s `Set.intersection` b) == 1) twoThreeFive)
    twoThree = filter (\s -> Set.size (s `Set.intersection` b) == 0) twoThreeFive
    two = head (filter (\s -> Set.size (s `Set.intersection` one) == 1) twoThree)
    three = head (filter (\s -> Set.size (s `Set.intersection` one) == 2) twoThree)
    -- decode using computed sets
    sets = [(0, zero), (1, one), (2, two), (3, three), (4, four), (5, five), (6, six), (7, seven), (8, eight), (9, nine)]
    decodedDigits = map (\e -> fst (head (filter (\(_, s) -> s == Set.fromList e) sets))) (outputPattern e)
    -- convert to decimal
    res = foldr (\d acc -> acc * 10 + d) 0 (reverse decodedDigits)

signalPatternSets :: [String] -> [Set.Set Char]
signalPatternSets = map Set.fromList

-- Part 1 stuff
countDigits :: [Int] -> [String] -> Int
countDigits pattern = foldr (\s acc -> let d = computeDigit s in (if isDigitMatch d pattern then acc + 1 else acc)) 0

isDigitMatch :: Maybe Int -> [Int] -> Bool
isDigitMatch Nothing _ = False
isDigitMatch (Just d) m = d `elem` m

computeDigit :: String -> Maybe Int
computeDigit s | length s == 2 = Just 1
computeDigit s | length s == 4 = Just 4
computeDigit s | length s == 3 = Just 7
computeDigit s | length s == 7 = Just 8
computeDigit _ = Nothing

-- Parsing code
parseEntries :: [String] -> [Entry]
parseEntries = map parseEntry

parseEntry :: String -> Entry
parseEntry s = Entry {signalPatterns = signal, outputPattern = output}
  where
    signalOutput = S.splitOn " | " s
    signal = S.splitOn " " (head signalOutput)
    output = S.splitOn " " (last signalOutput)
