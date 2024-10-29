module V20241029 where

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





