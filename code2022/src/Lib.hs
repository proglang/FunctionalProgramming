module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f b [] = b
foldr' f b (x : xs) = f x (foldr' f b xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f b [] = b
foldl' f b (x : xs) = foldl' f (f b x) xs

foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f b xs = foldr' (\a fb -> fb . (\z -> f z a)) id xs b
