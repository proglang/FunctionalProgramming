import Data.Char

type T1 = Int -> Int -> Int
type T2 = Int -> (Int -> Int)
type T3 = (Int -> Int) -> Int

f1 :: T2
f1 = undefined

pick 1 = fst
pick 2 = snd

type C2 = (Int, Int) -> Int

-- maps function :: C2 to function  :: T1
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

or', and' :: [Bool] -> Bool
or' xs = foldr (||) False xs

and' xs = foldr (&&) True xs

-- concat' [] =[]
-- concat' ["Poppy", "Seed"] = "PoppySeed"
concat' :: [[a]] -> [a]
concat' xss = foldr (++) [] xss

maximum' (x:xs) = foldr max x xs

-- removeSpaces "abc def \n ghi" == "abcdefghi"
removeSpaces :: String -> String
removeSpaces xs = filter (not . isSpace) xs

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g x = f (g x)
