module M07Laziness where

nat :: [Integer]

nat = 0 : map (+1) nat

nathelper i = i : nathelper (i+1)
nat'' = nathelper 0

nat' = [0..]

--0 cyclic representation in memory
ones :: [Integer]
ones = 1 : ones

stupid = stupid

repeat' :: a -> [a]
repeat' x = x : repeat' x
{-
repeat' 1 ==
  1 : repeat' 1 ==
  1 : 1 : repeat' 1
-}
-- cyclic representation in memory
repeat'' :: a -> [a]
repeat'' x = xs
  where xs = x : xs

-- fib(i+2) = fib (i+1) + fib (i)
-- fib !! (i+2) == fib !! (i+1) + fib !! i

-- fib (i+2) =  0  1  1  2  3  XX
-- fib (i+1) =     0  1  1  2  3
-- fib (i)   =        0  1  1  2  3


fib :: [Integer]
fib = 0 : 1 : zipWith (+) (tail fib) fib

primes :: [Integer]
primes = helper [2..]

helper (p:rest) =
  p : helper (filter (\n -> n `mod` p /= 0) rest)
  
-- hamming numbers

data BTree = Leaf Int | Branch BTree BTree
  deriving Show

mintree :: BTree -> BTree
mintree t =
  let m = minleaf t in
  replaceLeaves t m

minleaf :: BTree -> Int
minleaf (Leaf i) = i
minleaf (Branch l r) = minleaf l `min` minleaf r

replaceLeaves :: BTree -> Int -> BTree
replaceLeaves (Leaf _) i = 
  Leaf i
replaceLeaves (Branch l r) i = 
  Branch (replaceLeaves l i) (replaceLeaves r i)

-- just one traversal
mintree' :: BTree -> BTree
mintree' t = mt
  where (mt, m) = replaceMin t m


replaceMin :: BTree -> Int -> (BTree, Int)
replaceMin (Leaf i) m = (Leaf m, i)
replaceMin (Branch l r) m = (Branch rl rr, ml `min` mr)
  where (rl, ml) = replaceMin l m
        (rr, mr) = replaceMin r m
