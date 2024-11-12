module V20241105 where

import Data.Char

data BTree a =
  Leaf | Branch { left :: BTree a, item :: a, right :: BTree a }
  deriving (Show)

height :: BTree a -> Int
height Leaf = 0
height (Branch l _ r) = 1 + max (height l) (height r)

height' :: BTree a -> Int
height' Leaf = 0
height' (Branch {left = l, right = r}) = 1 + max (height' l) (height' r)

sumTree :: Num a => BTree a -> a
sumTree Leaf = 0
sumTree (Branch l i r) = i + sumTree l + sumTree r

----

foldBTree :: (b -> a -> b -> b) -> b -> BTree a -> b
foldBTree f z Leaf = z
foldBTree f z (Branch l i r) = f (foldBTree f z l) i (foldBTree f z r)

height'' t = foldBTree f z t
  where
    f l i r = 1 + max l r
    z       = 0

sumTree'' t = foldBTree f z t
  where
    f l i r = l + i + r
    z = 0

height'''  = foldBTree (\ l i r -> 1 + max l r) 0
sumTree''' = foldBTree (\ l i r -> l + i + r) 0


----

-- assume BTree a where Ord a is a binary search tree

node :: a -> BTree a
node x = Branch Leaf x Leaf

insert :: Ord a => BTree a -> a -> BTree a
insert Leaf x = node x
insert t@(Branch l i r) x =
  case compare x i of
    LT -> Branch (insert l x) i r
    EQ -> t
    GT -> Branch l i (insert r x)

search :: Ord a => BTree a -> a -> Bool
search Leaf x = False
search (Branch l i r) x =
  case compare x i of
    LT -> search l x
    EQ -> True
    GT -> search r x

---

data ATree k v =
  ALeaf
  | ABranch { aleft :: ATree k v, key :: k, value :: v, aright :: ATree k v }
  deriving (Show)



anode :: k -> v -> ATree k v
anode k v = ABranch ALeaf k v ALeaf

ainsert :: Ord k => ATree k v -> k -> v -> ATree k v
ainsert ALeaf k v = anode k v
ainsert t@(ABranch l i w r) k v =
  case compare k i of
    LT -> ABranch (ainsert l k v) i w r
    EQ -> ABranch l i v r
    GT -> ABranch l i w (ainsert r k v)

asearch :: Ord k => ATree k v -> k -> Maybe v
asearch ALeaf k = Nothing
asearch (ABranch l i w r) k =
  case compare k i of
    LT -> asearch l k
    EQ -> Just w
    GT -> asearch r k

----

ex1 :: (a -> b) -> Int
ex1 f = 42

ex2 :: (Int -> Int) -> Int
ex2 f = f (f 42)

----

-- pick :: Int -> ((a, a) -> a)
pick 1 = fst
pick 2 = snd

bpick False = fst
bpick True = snd


----
-- foldr examples

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = x `f` foldr' f z xs


-- concat ( xs1 : xs2 : ... : xsn : []) == xs1 ++ xs2 ++ ... ++ xsn ++ []
-- concat :: [[a]] -> [a]

concat' :: [[a]] -> [a]
concat' xs = foldr' (++) [] xs

-- equivalent "eta-reduced" function defintion
concat'' = foldr' (++) []

-- maximum ( x1 : x2 : ... : xn : []) == max x1 (max x2 (... (max xn minBound)))

maximum' :: (Bounded a, Ord a) => [a] -> a
maximum' = foldr' max minBound

exm :: Int
exm = maximum' [-10, 10, 42]

-----
isNotSpace :: Char -> Bool
isNotSpace = not . isSpace
-- a pointfree definition

-- (f . g) x = f (g x)

isNotSpace' x = (not . isSpace) x   -- can eta reduce

isNotSpace'' x = not (isSpace x)    -- cannot eta reduce

isReallySpace = not . not . isSpace
isReallySpace'' x = not (not (isSpace x)) -- cannot eta reduce


------------------------------------------------------------
-- laziness

nat :: [Integer]
nat = [0..]

nat' = 0 : map (+1) nat'

ones = 1 : ones

nat'' = 0 : zipWith (+) nat'' ones

repeat' x = x : repeat' x

repeat'' x = loop
  where loop = x : loop

-- infix function application
-- (f $ x) = f x

--  f . g $ h . k $ x
--  (f . g) ((h . k) x)

fib = 0: 1: zipWith (+) fib (tail fib)

fib' n = fibl !! n
  where
    fibl = 0: 1: zipWith (+) fibl (tail fibl)


fib'' () = fibl
    where
     fibl = 0: 1: zipWith (+) fibl (tail fibl)

primes = sieve [2..]
sieve (p: xs) = p: (sieve $ filter (\x -> x `mod` p /= 0) xs)
sieve' (p: xs) = p: (sieve' $ filter ((/= 0). (`mod` p)) xs)
sieve'' (p: xs) = p: sieve'' [ x | x <- xs, x `mod` p /= 0]

----------------------------------------
-- the minimum tree problem
----------------------------------------

data BinTree = BLeaf Int | Node BinTree BinTree
  deriving (Show)


mintree :: BinTree -> BinTree
mintree b = t
  where
    (t, m) = mtree b m

    mtree :: BinTree -> Int -> (BinTree, Int)
    mtree (BLeaf v) m = (BLeaf m, v)
    mtree (Node l r) m =
      let (tl, ml) = mtree l m
          (tr, mr) = mtree r m
      in  (Node tl tr, min ml mr)


----------------------------------------

data Color = Red | Black
  deriving (Eq, Show, Enum)

colors = [Red .. Black]

data Suit = Spades | Hearts | Diamond | Clubs
  deriving (Eq, Show, Enum)

-- error: ambiguity
-- ov x = show (read x)

vo x = read(show x)


