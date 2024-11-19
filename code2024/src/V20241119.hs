module V20241119 where

import Test.QuickCheck hiding (Sorted, orderedList)
import qualified Data.List as L

data Suit = Spades | Hearts | Diamonds | Clubs
  deriving (Show, Eq, Enum)

instance Arbitrary Suit where
  arbitrary = elements [Spades .. Clubs]

data Rank = Numeric Integer | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord)

instance Arbitrary Rank where
  arbitrary = frequency [ (4, elements [Jack, Queen, King, Ace])
                        , (9, choose (2,10) >>= return . Numeric)
                        ]

data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Show)

instance Arbitrary Card where
  arbitrary = do
    r <- arbitrary
    s <- arbitrary
    return $ Card r s

gen_even :: Gen Integer
gen_even = do
  i <- arbitrary
  return (2 * i)

gen_even' :: Gen Integer
gen_even' = 
  arbitrary >>= return . (2*)

gen_nonneg :: Gen Integer
gen_nonneg = 
  arbitrary >>= return . abs

validRank :: Rank -> Bool
validRank (Numeric n) = 2 <= n && n <= 10
validRank _ = True

prop_all_valid_rank_collect r = collect r (validRank r)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [ x ]
insert x (y : ys) | x <= y = x : (y : ys)
               | otherwise = y : insert x ys

isOrdered :: Ord a => [a] -> Bool
isOrdered [] = True
isOrdered [ _ ] = True
isOrdered (x : y : xs) = x <= y && isOrdered (y : xs)

prop_insert_1 :: Integer -> [Integer] -> Bool
prop_insert_1 x xs = isOrdered (insert x xs)

prop_insert_2 :: Integer -> [Integer] -> Bool
prop_insert_2 x xs = not (isOrdered xs) || isOrdered (insert x xs)


orderedList :: (Arbitrary a, Ord a) => Gen [a]
orderedList = do
  xs <- arbitrary
  return $ L.sort xs

newtype Sorted a =
  Sorted [a]
  deriving (Show)

instance (Ord a, Arbitrary a) => Arbitrary (Sorted a) where
  arbitrary = do
    sxs <- orderedList
    return $ Sorted sxs

prop_insert_3 :: Integer -> Sorted Integer -> Bool
prop_insert_3 x (Sorted xs) = isOrdered (insert x xs)

