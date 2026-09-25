{-# LANGUAGE QualifiedDo #-}
module V11 where

-- import SimplePrelude as S
-- import Prelude ()

  
ap :: Monad m => m (a -> b) -> m a -> m b
ap mfab ma = do
  f <- mfab
  a <- ma
  return (f a)

-- different presentation for monads...
-- instead of the bind operation
-- you have a `join` operation

join :: Monad m => m (m a) -> m a
join mma = do
  ma <- mma
  ma

----------------------------------------------------

type Parser tok res = [tok] -> [(res, [tok])]

-- pempty recognizes the empty language
pempty :: Parser tok res
pempty ts = []

-- succeed recognizes the empty string

succeed :: r -> Parser tok r
succeed r = \ts -> [(r, ts)]

-- satisfy checks first token using a predicate `p`

satisfy :: (t -> Bool) -> Parser t t
satisfy p (t : ts) | p t = [(t, ts)]
satisfy p _ = []

-- msatisfy checks and returns a substitute for tok

msatisfy :: (t -> Maybe r) -> Parser t r
msatisfy p (t : ts) =
  case p t of
    Just r -> [(r, ts)]
    Nothing -> []
msatisfy p [] = []

-- Haskell feature: pattern guards

msatisfy' :: (t -> Maybe r) -> Parser t r
msatisfy' p (t : ts) | Just r <- p t = [(r, ts)]
msatisfy' _ _ = []

-- check literal occurrence of a topken

lit :: Eq t => t -> Parser t t
lit t = satisfy (== t)

-- alternative

palt :: Parser t r -> Parser t r -> Parser t r
palt p1 p2 ts = p1 ts ++ p2 ts

-- sequence

pseq :: Parser t (s -> r) -> Parser t s -> Parser t r
pseq pf pa ts =
  [ (f a, ts'')
  | (f, ts') <- pf ts, (a, ts'') <- pa ts' ]

-- map

pmap :: (s -> r) -> Parser t s -> Parser t r
pmap f p ts = [ (f a, ts') | (a, ts') <- p ts ]

--

instance Functor (Parser t) where
  
