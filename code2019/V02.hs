module V02 where

import Data.Complex
import Data.List (genericLength)

-- |compute x to the n-th power
power :: Complex Double -> Integer -> Complex Double
power x 0 = 1
power x n | n > 0 = x * power x (n - 1)

examplePair :: (Double, Bool)
examplePair = (3.14, True)

exampleTriple :: (Bool, Integer, String)
exampleTriple = (False, 42, "hello, world")

exampleFunction :: (Bool, Integer, String ) -> Bool
exampleFunction (b, i, s) = b || fromInteger i > length s
-- alternative: ... = b || i > genericLength s


exampleTriple' :: (Bool, Int, String)
exampleTriple' = (False, 42, "hello, world")

exampleFunction' :: (Bool, Int, String ) -> Bool
exampleFunction' (b, i, s) = b || i > length s

summerize :: [String] -> String
summerize [] = "None"
summerize [x] = "Only " ++ x
summerize [x,y] = "Two things: " ++ x ++ " and " ++ y
summerize [_ ,_ ,_ ] = "Three things: ???"
summerize _ = "Several things."

-- |doubles every element of a list of numbers
doubles :: [Integer] -> [Integer]
doubles [] = []
doubles (x:xs) = (2 * x) : doubles xs

-- |map a function on all elements of a list
mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = f x : mymap f xs

double x = 2 * x

doubles' xs = mymap double xs

-- task: use quickcheck to compare doubles and doubles'


 
