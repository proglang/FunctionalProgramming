module M10monad where

main = do
  putStrLn "Hello world."
  "" <- return "HI there"
  return ()
{-
-- 1. monad law
-- return x >>= f == f x

m1 x f = Just x >>= f

-- 2. monad law
-- m >>= return == m

m2 m = (Nothing >>= return)
     == Nothing
m3 m x = (Just x) >>= return 
       == Just x
-}
{-
instance Monad [] where
  return x = [x]
  m >>= f = concatMap f m
-}

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- for lists
-- (>>=) :: [a] -> (a -> [b]) -> [b]

-- concatMap :: (a -> [b]) -> [a] -> [b]

-- mf :: [a] -> (a -> [b]) -> ...

mf m f = map f m

concatMap'  :: (a -> [b]) -> [a] -> [b]
concatMap' f xs = concat $ map f xs

concatMap'' f = concat . map f

-- doesn't work:
-- concatMap''' = concat . map
