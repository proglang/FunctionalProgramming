module V07 where

ones :: [Integer]
ones = 1 : ones

repeat' :: a -> [a]
repeat' x = x : repeat' x

repeat'' :: a -> [a]
repeat'' x = xs
  where xs = x : xs

nfib :: Integer -> Integer
nfib n | n == 0 = 0
       | n == 1 = 1
       | otherwise = nfib (n-1) + nfib (n-2)

nfib1000 = nfib 30

fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)
  --- fib (n+2) = fib (n+1) + fib n
  --- fib_0 = zipWith (+) fib_1 fib_2

badfib = 1 : zipWith (+) badfib (tail badfib)

primes :: [Integer]
primes = sieve [2..]

sieve (p : xs) = p : sieve (filter (\x -> x `mod` p /= 0) xs)

data BTree = Leaf Integer | Branch BTree BTree
  deriving (Show)

mintree :: BTree -> BTree
mintree b = mb
  where
    (ib, mb) = helper ib b
    helper :: Integer -> BTree -> (Integer, BTree)
    helper m (Leaf i) = (i, Leaf m)
    helper m (Branch l r) =
      let (il, ml) = helper m l
          (ir, mr) = helper m r
      in  (min il ir, Branch ml mr)
