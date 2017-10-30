import Test.QuickCheck

data List a = Empty | Next a (List a)

intoList :: [a] -> List a
intoList [] = Empty
intoList (x:xs) = Next x (intoList xs)

-- append (++), reverse
l1 = [1,2,3]
l2 = [4,5]

-- insert for insertion sort
insert' x [] = [x]
insert' y (x:xs) | y <= x = y : x : xs
                 | otherwise = x : (insert' y xs)

-- insertion sort
isort [] = []
isort (x:xs) = insert' x (isort xs)

-- quick sort
qsort [] = []
qsort (x:xs) = qsort (filter (<x) xs) ++ (x : qsort (filter (>=x) xs))

-- split according to predicate
split :: (a -> Bool) -> [a] -> ([a], [a])
split p []     = ([], [])
split p (x:xs) | p x = (x:yess, nos)
               | otherwise = (yess, x:nos)
    where (yess, nos) = split p xs

-- qsort using split
qsort' [] = []
qsort' (x:xs) = qsort' smaller ++ x : qsort' larger
    where (smaller, larger) = split (<x) xs


-- quickcheck pitfall
prop_take_drop, nonprop_take_drop :: Eq a => Int -> [a] -> Bool
prop_take_drop n xs = take n xs ++ drop n xs == xs
nonprop_take_drop n xs = drop n xs ++ take n xs == xs