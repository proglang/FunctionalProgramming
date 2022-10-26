module V03 where

doubles :: [Integer] -> [Integer]
doubles [] = []
doubles (x:xs) = 2*x : doubles xs

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

doubles' :: [Integer] -> [Integer]
doubles' xs = map' (2 *) xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) | p x = x : filter' p xs
           | otherwise = filter' p xs

-- alternative
-- filter' p (x:xs) = if px then ...

data Suit = Spades | Hearts | Diamonds | Clubs
  deriving (Show)

data Color = Red | Black
  deriving (Show)

color :: Suit -> Color
color Spades = Black
color Hearts = Red
color Diamonds = Red
color Clubs = Black

--------------------------------------------

data Two = One | Two

-- pick :: Two -> (a, a) -> a
pick One = fst
pick Two = snd

pick' n | n == 1 = fst
        | n == 2 = snd
