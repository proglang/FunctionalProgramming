import Data.List
import Test.QuickCheck

-- |A property to test the binomial formula
prop_binomi :: Integer -> Integer -> Bool
prop_binomi a b = (a + b) ^ 2 == a ^ 2 + 2 * a * b + b ^ 2

{-
elements  :: [a] -> Gen a
oneof     :: [Gen a] -> Gen a
frequency :: [(Int,Gen a)] -> Gen a
listOf    :: Gen a -> Gen [a]
vectorOf  :: Int -> Gen a -> Gen [a]
choose    :: Random a => (a,a) -> Gen a
-}


str1 = elements "ABCDEF"
str2 = elements "PQ"

str12 = oneof [str1, str2]

fstr12 = frequency [(6,str1), (2,str2)]

data Suit = Spades | Hearts | Diamonds | Clubs
     deriving (Show, Eq)

gSuit :: Gen Suit
gSuit = elements [Spades, Hearts, Diamonds, Clubs]

instance Arbitrary Suit where
  arbitrary = gSuit

data Rank = Numeric Integer | Jack | Queen
          | King | Ace
          deriving (Show, Eq, Ord)

gImage :: Gen Rank
gImage = elements [Jack, Queen, King, Ace]

gRank :: Gen Rank
gRank = frequency [(4, gImage), (9, gNumerics)]

data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Eq, Show)

gCard :: Gen Card
gCard = do
  s <- gSuit
  r <- gRank
  return $ Card r s
  


gEven :: Gen Integer
gEven = arbitrary >>= (return . (*2))

gNonneg :: Gen Integer
gNonneg = arbitrary >>= (return . abs)

data Hand = One Card | More Card Hand
  deriving (Eq, Show)

gHand :: Gen Hand
gHand = oneof 
  [gCard >>= (return . One)
  ,gCard >>= \c -> gHand >>= \h -> return $ More c h]
  

---
gNumerics' :: Gen Integer
gNumerics' = choose (2, 10)

gNumerics :: Gen Rank
gNumerics = gNumerics' >>= \n -> return (Numeric n)

gNumerics1 = gNumerics' >>= \n -> (return . Numeric) n
gNumerics2 = gNumerics' >>= (return . Numeric)

gNumerics0 = gNumerics' >>= \n -> return $ Numeric n

-- f $ x = f x

----

validRank :: Rank -> Bool
validRank (Numeric n) = 2 <= n && n <= 10
validRank _ = True
prop_all_validRank = forAll gRank validRank


instance Arbitrary Rank where
  arbitrary = gRank

prop_all_valid_rank_collect r = collect r (validRank r)

instance Arbitrary Hand where
  arbitrary = gHand

size (One _) = 1
size (More _ h) = 1+size h

prop_all_hand_collect :: Hand -> Property
prop_all_hand_collect h = collect (size h) True



-- |Check if a list is ordered
isOrdered :: Ord a => [a] -> Bool
isOrdered [] = True
isOrdered [_] = True
isOrdered (x1:x2:xs) = x1 <= x2 && isOrdered (x2:xs)

prop_insert_1 :: Integer -> [Integer] -> Bool
prop_insert_1 x xs = isOrdered (insert x xs)

prop_insert_2 :: Integer -> [Integer] -> Property
prop_insert_2 x xs = isOrdered xs ==> isOrdered (insert x xs)

prop_insert_3 x =
    forAll orderedList (\xs->isOrdered (insert x xs))
    
