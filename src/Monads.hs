module Monads (Tree (..), ZipList (..), Expr (..), ST (S), app, tree1, rlabel, fresh, alabel) where

import Control.Monad

-- 12.5 exercises

-- 1. Define an instance of the functor class for the following type of binary trees
-- that have data in their nodes.
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

-- 2. Complete the following instance declaration to make the partially-applied function type (a ->) into a functor.
-- Commented due to conflict with Base lib
-- instance Functor ((->) a) where
-- fmap :: (b -> c) -> (a -> b) -> (a -> c)
-- fmap = (.)

-- 3. Define an instance of the applicative class for partially applied function type (a ->).
-- Commented due to conflict with Base lib
-- instance Applicative ((->) a) where
-- pure :: b -> (a -> b) (K combinator I think) (also the type is b -> a -> b as the by default it associates to the right)
-- pure f _ = f
-- const x is a unary function which evaluates to x for all inputs.
-- pure = const

-- (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c) (S combinator I think)
-- f <*> g = \x -> f x (g x)
-- Semantically same as GHC.Base implementation
-- (<*>) f g x = f x (g x)

-- 4. Ziplist re-implement the Control.Applicative zippy instance for lists.
newtype ZipList a = Z [a]
  deriving (Show)

instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (map g xs)

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z (repeat x)

  -- (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z [g x | (g, x) <- zip gs xs]

-- 5. Work out types for the 4 applicative laws.
-- Identity
-- pure id <*> v = v

-- Composition
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

-- Homomorphism
-- pure f <*> pure x = pure (f x)

-- Interchange
-- u <*> pure y = pure ($ y) <*> u

-- 6. Monad class for type (a ->)
-- instance Monad ((->) a) where
-- return :: a -> m a
-- return : b -> (a -> b)
-- return = pure

-- (>>=) :: ((->) a) -> (a -> ((->) b)) -> ((->) b)
-- f >>= k = \a -> k (f a) a

-- 7.
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving (Show)

instance Functor Expr where
  -- fmap :: (a -> b) -> Expr a -> Expr b
  fmap f (Var x) = Var (f x)
  fmap f (Val x) = Val x
  fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
  -- pure :: a -> Expr a
  pure x = Var x

  -- <*> :: f (a -> b) -> f a -> f b
  -- <*> :: Expr (a -> b) -> Expr a -> Expr bj
  -- Not bothering with other patterns.
  Var g <*> ex = fmap g ex

instance Monad Expr where
  -- return :: a -> Expr a
  return = pure

  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  Var x >>= f = f x
  Val x >>= _ = Val x
  Add x y >>= f = Add (x >>= f) (y >>= f)

-- 8. State Transformer Monad
type State = Int

newtype ST a = S (State -> (a, State))

-- Special purpose applicator function (to remove the dummy S type)
app :: ST a -> State -> (a, State)
app (S st) = st

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = do
    x <- st
    S (\s -> ((g x), s))

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x, s))

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = do
    f <- stf
    x <- stx
    S (\s -> (f x, s))

instance Monad ST where
  -- return :: a -> ST a
  return = pure

  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')

-- Test the ST with the tree labelling example
tree1 :: Tree Char
tree1 = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- Relabeling - non ST version
-- lots of plumbing code.
rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf x) n = (Leaf n, n + 1)
rlabel (Node l r) n = (Node l' r', n'')
  where
    (l', n') = rlabel l n
    (r', n'') = rlabel r n'

-- ST version
fresh :: ST Int
fresh = S (\n -> (n, n + 1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r
