module V20241203 where

-- instance Functor ((->) a) where
--   fmap :: (c -> d) -> (a -> c) -> (a -> d)
--   fmap f g = f . g

type Parser t r = [t] -> [(r, [t])]

-- recognizes the empty language
pempty :: Parser t r
pempty ts = []

-- recognizes the language with just the empty word
-- (looks like return or pure!)
succeed :: r -> Parser t r
succeed r ts = [(r, ts)]

-- `satisfy p` recognizes the language { a | p a }
satisfy :: (t -> Bool) -> Parser t t
satisfy p (t: ts) | p t = [(t, ts)]
satisfy p _ = []

-- variation of above
msatisfy :: (t -> Maybe r) -> Parser t r
msatisfy m (t: ts) | Just r <- m t = [(r, ts)]
msatisfy m _ = []

-- `lit t` recognizes { t }
lit :: Eq t => t -> Parser t t
lit t = satisfy (== t)

-- alternative of parsers
palt :: Parser t r -> Parser t r -> Parser t r
palt p1 p2 ts = p1 ts ++ p2 ts

-- sequence of parsers
pseq :: Parser t (a -> b) -> Parser t a -> Parser t b
pseq p1 p2 ts = [ (f a, ts2) | (f, ts1) <- p1 ts, (a, ts2) <- p2 ts1]

pmap :: (s -> r) -> (Parser t s -> Parser t r)
pmap f p ts = [ (f s, ts) | (s, ts) <- p ts ]
