module V02 where

absolute :: Integer -> Integer
absolute x | x >= 0    = x
           | otherwise = -x

power :: (Num a) => a -> Integer -> a
power x n = if n == 0 then 1 else x * power x (n - 1)
