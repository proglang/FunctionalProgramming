-- curried function type: a -> b -> c
-- uncurried function type: (a, b) -> c

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f ab = f (fst ab) (snd ab)

curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' g a b = g (a , b)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

or' :: [Bool] -> Bool
or' xs = foldr' (||) False xs

and' :: [Bool] -> Bool
and' xs = foldr' (&&) True xs

g xs = foldr' (:) [] xs

append xs ys = foldr' (:) ys xs

-- (.) f g x = f (g x)

-- Eta reduction:
-- \ x -> e x --> e
-- where x not free in e

-- f x y = g x y
-- -->
--  f = g

-- term rewriting example

-- f (i (e), f (e, e))
-->
-- f (f (i (e), e), e)
-->  [ x -> e ]
-- f (e, e)
-->  [ x -> e ]
-- e


