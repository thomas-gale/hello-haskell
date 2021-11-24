-- A giant pile of experimental garbage.
module LearnLib
  ( hello,
    doubleMe,
    doubleUs,
    Pos,
    origin,
    left,
    Answer (..), -- Export data type and all constructors
    flipAns,
    Shape (..),
    square,
    area,
    isAllDigits,
    BTree (..),
    tree1,
  )
where

-- hello world
hello :: IO ()
hello = putStrLn "Hello Haskell"

-- baby steps
doubleMe x = x + x

doubleUs x y = x * 2 + y * 2

-- declare type alias (abbreviation/synonym) - must always begin with capital letter.
-- is a composition of already existing types.
type Pos n = (n, n)

origin :: Pos Int
origin = (0, 0)

left :: Pos Int -> Pos Int
left (x, y) = (x - 1, y)

-- declare data (completely new type).
-- Yes, No etc. are constructors for the type - must always begin with capital letter.
data Answer = Yes | No | Unknown
  deriving (Show)

answers :: [Answer]
answers = [Yes, No, Unknown]

flipAns :: Answer -> Answer
flipAns Yes = No
flipAns No = Yes
flipAns Unknown = Unknown

-- Constructors can have parameters. Behind the scenes constructors are functions that return a value of the data type.
data Shape = Circle Float | Rect Float Float
  deriving (Show)

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y

isAllDigits :: [Char] -> Bool
isAllDigits "" = False
isAllDigits val = all ((== True) . (\x -> x `elem` ['1', '2', '3', '4', '5', '6', '7', '8', '9'])) val

-- Recursive data types.
data BTree a = Empty | BTree a (BTree a) (BTree a)
  deriving (Show)

tree1 = BTree 2 (BTree 7 (BTree 2 Empty Empty) (BTree 6 Empty Empty)) (BTree 5 Empty (BTree 9 (BTree 4 Empty Empty) Empty))