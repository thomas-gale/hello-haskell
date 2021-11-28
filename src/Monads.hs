module Monads (Tree (..)) where

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
-- pure :: b -> (a -> b) (K combinator I think)
-- pure f _ = f
-- pure = const

-- (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c) (S combinator I think)
-- f <*> g = \x -> f x (g x)


