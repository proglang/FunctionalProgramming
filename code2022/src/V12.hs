{-# LANGUAGE QualifiedDo #-}
module V12 where

import SimplePrelude as S
import Prelude ()

f :: (Int, Int) -> (Int, Int)
g :: (Int, Int) -> Int

f' :: (Int, Int) -> Int
g' :: Int -> Int

f (x, y) = (2*x, 2*y)
g (x, y) = x

f' (x, y) = 2*x
g' x = x

-- g . f == g' . f'

g'' x = 3 * x

instance Functor [] where
  fmap f [] = []
  fmap f (x : xs) = f x : fmap f xs

-- functorial law #1
-- fmap id_A == id_list_A
-- to show that we invoke extensionality
-- for all xs :: [a]
-- show `fmap id xs == xs`
-- requires inductive proof
{-
    fmap id [] == []   (by def of fmap)
    fmap id (x : xs)
==  id x : fmap id xs  (by IH)
==  x : xs
==  id (x : xs)
-}

instance Functor Maybe where
  -- fmap :: (a -> b) -> (Maybe a -> Maybe b)
  fmap f Nothing = Nothing
  fmap f (Just x) = Just (f x)

{-
fmap id Nothing == Nothing
fmap id (Just x) == Just (id x) == Just x

-}

data BTree a = Leaf | Node (BTree a) a (BTree a)

instance Functor BTree where
  -- fmap :: (a -> b) -> (BTree a -> BTree b)
  fmap f Leaf = Leaf
  fmap f (Node l x r) =
    Node (fmap f l) (f x) (fmap f r)

-- some predefined functor instances

instance Functor ((,) a) where
  -- we consider the object map, for any type c
  -- f : a |-> (c, a)
  -- the arrow part
  -- fmap :: (a -> b) -> ((c, a) -> (c, b))
  fmap f (c, a) = (c, f a)

data Pair a b = Pair a b
-- can define functor a |-> Pair c a (in Haskell)
-- there is also a functor a |-> Pair a c

data Diag a = Diag a a
instance Functor Diag where
  fmap f (Diag x y) = Diag (f x) (f y)

instance Functor ((->) r) where
  -- strongly related to the reader monad
  -- object part
  -- f : a -> (r -> a)
  -- arrow part
  -- fmap :: (a -> b) -> ((r -> a) -> (r -> b))
  fmap f ra = f . ra

{- just for you information:
   what about the object mapping
   f : a -> (a -> r)
   proposal for corresponding arrow mapping
   fmap :: (a -> b) -> ((a -> r) -> (b -> r))
   fmap f ar = ???
   doesn't work this way!
   instead, we need an fmap' of a different type
   fmap' :: (a -> b) -> ((b -> r) -> (a -> r))
   fmap' f br = br . f

   different kind of functor, a so-called
   *contravariant functor* where
   fmap' :: (a -> b) -> (f b -> f a)
-}
