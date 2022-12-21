module V13 where

import Data.Char

newtype Parser tok res = Parser ([tok] -> [(res, [tok])])
exParser (Parser p) = p

-- pempty recognizes the empty language
pempty :: Parser tok res
pempty = Parser $ \ts -> []

-- succeed recognizes the empty string

succeed :: r -> Parser tok r
succeed r = Parser $ \ts -> [(r, ts)]

-- satisfy checks first token using a predicate `p`

satisfy :: (t -> Bool) -> Parser t t
satisfy p = Parser helper
  where
    helper (t : ts) | p t = [(t, ts)]
    helper _ = []

-- msatisfy checks and returns a substitute for tok

msatisfy :: (t -> Maybe r) -> Parser t r
msatisfy p = Parser helper
  where
    helper (t : ts) =
      case p t of
        Just r -> [(r, ts)]
        Nothing -> []
    helper [] = []


-- check literal occurrence of a topken

lit :: Eq t => t -> Parser t t
lit t = satisfy (== t)

-- alternative

palt :: Parser t r -> Parser t r -> Parser t r
palt p1 p2 = Parser $ \ts -> exParser p1 ts ++ exParser p2 ts

-- sequence

pseq :: Parser t (s -> r) -> Parser t s -> Parser t r
pseq pf pa =
  Parser $ \ts ->
  [ (f a, ts'')
  | (f, ts') <- exParser pf ts
  , (a, ts'') <- exParser pa ts' ]

-- map

pmap :: (s -> r) -> Parser t s -> Parser t r
pmap f p = Parser $ \ts -> [ (f a, ts') | (a, ts') <- exParser p ts ]

--

instance Functor (Parser t) where
  fmap = pmap

instance Applicative (Parser t) where
  -- pure :: a -> Parser t a
  pure = succeed
  -- (<*>) :: Parser t (s -> r) -> Parser t s -> Parser t r
  (<*>) = pseq

instance Monad (Parser t) where
  -- Parser t a -> (a -> Parser t b) -> Parser t b
  pa >>= fb = Parser $ \ts ->
    [ (b, ts'')
    | (a, ts') <- exParser pa ts
    , (b, ts'') <- exParser (fb a) ts'
    ]

-- need a parser for repetitions

many :: Parser t r -> Parser t [r]
many p =
  do r <- p
     rs <- many p
     return (r : rs)
  `palt`
  succeed []

-- ast of arithmetic expressions

data Term = Con Integer | Bin Term Op Term
  deriving (Eq, Show)
data Op = Add | Sub | Mul | Div
  deriving (Eq, Show)

-- a parser for positive integers

digitValue :: Char -> Int
digitValue x = ord x - 48

pdigit :: Parser Char Int
pdigit = fmap digitValue $ satisfy isDigit

pdigits :: Parser Char String
pdigits = do
  x <- satisfy isDigit
  xs <- many (satisfy isDigit)
  return (x : xs)

pnum :: Parser Char Integer
pnum = pmap read pdigits

pterm :: Parser Char Term
pterm = pmap Con pnum

pterm' = (pure Con <*> pnum)
         `palt`
         (pure Bin <*> pterm' <*> pop <*> pterm')

mop :: Char -> Maybe Op
mop '+' = Just Add
mop '-' = Just Sub
mop '*' = Just Mul
mop '/' = Just Div
mop _ = Nothing

pop :: Parser Char Op
pop = msatisfy mop
