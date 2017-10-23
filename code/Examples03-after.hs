import Test.QuickCheck

-- |examples with tuples
examplePair :: (Double, Bool)  -- Double x Bool
examplePair = (3.14, False)

exampleTriple :: (Bool, Int, String) -- Bool x Int x String
exampleTriple = (False, 42, "Answer")

exampleFunction :: (Bool, Int, String) -> Bool
exampleFunction (b, i, s) = not b && length s < i

firstPair (x, y) = x


























































-- |function over lists - examples
summerize :: [String] -> String
summerize []  = "None"
summerize ["Hasso"] = "Just for Hasso!!!"
summerize [x] = "Only " ++ x
summerize [x,y] = "Two things: " ++ x ++ " and " ++ y
summerize [_,_,_] = "Three things: ???"
summerize _   = "Several things."   -- wild card pattern




















































-- |double every value in a list
-- doubles [3,6,12] == [6,12,24]
doubles :: [Integer] -> [Integer]
doubles []     = []
doubles (x:xs) = (2*x) : doubles xs




















































-- |apply a function to all elements of a list
-- map' f [x1, x2, ..., xn] = [f x1, f x2, ..., f xn]
map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs


-- |doubles using map
doubles' xs = map' double xs

double :: Integer -> Integer
double x = 2*x

prop_double_is doubles' xs = doubles xs == doubles' xs

doubles'' xs = map' (*2) xs

















































-- |keep only elements in list that fulfill a predicate
filter' :: (a -> Bool) -> [a] -> [a]
filter' p []     = []
filter' p (x:xs) = if p x then x : filter' p xs else filter' p xs

xm3_is_1 x = mod x 3 == 1
-- equivalently
xm3_is_1' x = x `mod` 3 == 1

(%$%$) x m = x `mod` m == 1


filter'' p []     = []
filter'' p (x:xs) | p x = x : filter'' p xs
                  | otherwise = filter'' p xs

filter''' p []     = []
filter''' p (x:xs) | p x = x : fpxs
                   | otherwise = fpxs
    where fpxs = filter''' p xs
















































-- | twice


















































-- |foldr' (predefined)
