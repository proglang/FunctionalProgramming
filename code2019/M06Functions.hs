module M06Functions where

pick :: Int -> (component, component) -> component
pick 1 = fst
pick 2 = snd

type T1 = Int -> Int -> Int
type T2 = (Int, Int) -> Int

type T1' a b c = a -> b -> c
type T2' a b c = (a, b) -> c

-- T1 is the same as T1' Int Int Int

-- curry' :: T2' a b c -> T1' a b c

curry' :: ((a, b) -> c) -> (a -> b -> c)
curry'    f                 a    b = f (a,b)

uncurry'  :: (a -> b -> c) -> ((a, b) -> c)
uncurry'     f                 (a, b) = f a b

add :: T1
add x y = x+y

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' op e [] = e
foldr' op e (x:xs) = x `op` foldr' op e xs

or' xs = foldr' (||) False xs
and' xs = foldr' (&&) True xs
concat' xss = foldr' (++) [] xss

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' op [x] = x
foldr1' op (x:xs) = x `op` foldr1' op xs

maximum' xs = foldr1' max xs


snoc :: a -> [a] -> [a]
snoc x ys = foldr' (:) [x] ys

f3 xs = foldr' snoc [] xs

f4 f xs = foldr' fc [] xs
  where fc x ys = f x : ys

-- my f xs == f4 f xs

my f [] = [] -- = foldr' fc [] []
my f (x:xs) = f x : my f xs
--  foldr' fc [] (x:xs) =
--  fc x (foldr' fc [] xs) =
--  f x : foldr' fc [] xs =
  
f3' xs = foldr' (\ x ys -> (++[x]) ys) [] xs
f3'' = foldr' (\ x -> (++[x])) [] 

filter' p = foldr' op []
  where
    op x | p x = (x :)
         | otherwise = id
