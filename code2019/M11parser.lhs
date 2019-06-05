> module M11parser where
> import Data.Char
> import Data.List hiding (transpose)
> import Control.Monad
> import Control.Applicative

* Parsing

> data Term  = Con Integer
>            | Bin Term Op Term  
>              deriving (Eq, Show)
>            
> data Op    = Add' | Sub' | Mul' | Div'
>              deriving (Eq, Show)








** A parser maps a list of tokens to a result.

> type ParserV1 token result = [token] -> result
> type TermParser = ParserV1 Char Term











** A parser may not consume the entire input.

> type ParserV2 token result = [token] -> (result, [token])

Returns the result on a prefix of the input paired with the remaining input.

"3+46/7"  --> (Con 3, "+46/7")



** A parser may be non-deterministic:
Consider the grammar

A -> aA
A -> aB
B -> b

Represent using "list of successes" technique

> data Parser token result =
>   Parser {runParser :: [token] -> [(result, [token])]}

** parser :: Parser token result
out = runParser parser ts
recognizes ts if the list  out   contains   (r, [])
==> all input ts has been consumed and the result is r


* Combinator parser

Approach to construct parsers using a library of
primitive parsers and combining forms for parsers.
Operators often chosen so that parsers resemble
context-free grammars.



** Primitive parsers

*** pempty recognizes the empty language

> pempty :: Parser t r
> pempty = Parser $ \ts -> []

*** succeed recognizes the empty word

> succeed :: r -> Parser t r
> succeed r = Parser $ \ ts -> [(r, ts)]

*** satisfy p   recognizes   [t]  if  (p t)  holds

> satisfy :: (t -> Bool) -> Parser t t
> satisfy p = Parser g
>   where g (t:ts) | p t = [(t, ts)]
>         g _ = []

*** alternative: msatisfy f
recognizes the same language as satisfy, but transforms the token directly

> msatisfy :: (t -> Maybe a) -> Parser t a
> msatisfy f = Parser g
>   where g (t:ts) = case f t of
>                       Nothing -> []
>                       Just a  -> [(a, ts)]
>         g _ = []

*** lit t     recognizes   [t] 

> lit :: Eq t => t -> Parser t t
> lit t = satisfy (== t)

** Combination of parsers

*** Alternative <|> recognizes the union of two languages

> palt :: Parser t r -> Parser t r -> Parser t r
> palt p1 p2 =
>   Parser $ \ts -> runParser p1 ts ++ runParser p2 ts

*** Sequence <*> recognizes the concatenation of two languages

> pseq :: Parser t (s -> r) -> Parser t s -> Parser t r
> pseq p1 p2 = Parser $ \ts ->
>   let srs = runParser p1 ts -- :: [(s -> r, [t])]
>       g (srf, ts1) = map (h srf) $ runParser p2 ts1 -- [(s, [t])]
>       h srf (s, ts2) = (srf s, ts2)
>       rs  = map g srs -- [[(r,[t])]]
>   in  concat rs

> pseq' :: Parser t r1 -> Parser t r2 -> Parser t (r1, r2)
> pseq' = undefined

example

> --pex1 = pseq (lit 'a') (lit 'b')
> pex1 = pseq (msatisfy (mlit 'a')) (lit 'b')
> mlit :: Char -> Char -> Maybe (Char -> Char)
> mlit c d | c == d = Just id
>          | otherwise = Nothing


*** Mapping: pmap f p  recognizes  the same language as p,
          but changes the result

> pmap :: (s -> r) -> Parser t s -> Parser t r
> pmap f p = Parser (map g . runParser p)
>   where g (s, ts) = (f s, ts)


revisit example

> pex2 = pseq (pmap (const (const ())) 
>                   (lit 'a'))
>             (lit 'b')
>
> pex3 = pmap (const ()) $ lit 'c'
> pex4 = palt pex2 pex3

** the example grammar

A -> aA
A -> aB
B -> b

> pA :: Parser Char String
> pA = palt (pseq (pmap (:) (lit 'a')) pB)
>           (pseq (pmap (:) (lit 'a')) pA)
> pB :: Parser Char String
> pB = pmap return $ lit 'b'

A -> aA
A -> aB
B -> bB
B -> 


* An application to parsing expressions

Consider the grammar

E -> E "+" T
E -> T

pE = pseq pE (pseq (lit '+') pT)

E -> T E'
E' -> "+" T E'
E' -> 
T -> "let" Ident "=" E "in" E
T -> Ident
T -> Number
T -> "(" E ")"

x + 5 - z + 3
((x + 5) - z) + 3

** A problem
parseE = palt (pseq parseE ...) parseT
defines a parser that never terminates on any input

Transform the grammar; eliminate left recursion

** Abstract Syntax Trees (AST)
A datatype to represent the result of parsing

> data E = V String | N Integer | A E E | L String E E
>   deriving (Eq, Show)


** To start of, we need a lexer that partitions the incoming list of
characters into a list of tokens. A token is either a single symbol,
an identifier, or a number. Whitespace characters are removed.

> data Token = Symbol Char | Ident String | Number Integer
>   deriving (Eq, Show)

> lexer :: String -> [Token]
> lexer [] = []
> lexer (c:cs) 
>   | isSpace c = lexer cs
>   | isAlpha c = Ident (c:acs) : lexer acs_rest
>   | isDigit c = Number (read $ c:ncs) : lexer ncs_rest
>   | otherwise = Symbol c : lexer cs
>   where acs = takeWhile isAlphaNum cs
>         acs_rest = dropWhile isAlphaNum cs
>         ncs = takeWhile isDigit cs
>         ncs_rest = dropWhile isDigit cs

> numberToken n (Number m:_) = m == n
> numberToken n _ = False


> mNumberToken (Number n) = Just n
> mNumberToken _ = Nothing
> mIdentToken (Ident i) = Just i
> mIdentToken i = Nothing

> parserE :: Parser Token E
> parserE' :: Parser Token [E]
> parserT :: Parser Token E

> pIdent :: Parser Token String
> pIdent = msatisfy mIdentToken

> parserE  = pseq (pmap build_term parserT) parserE'
> parserE' = palt (pseq (pmap (const id) (lit $ Symbol '+'))
>                       (pseq (pmap (:) parserT) parserE'))
>                 (succeed [])
> parserT  = palt (pseq (pseq (pseq (pmap (const L) $ lit $ Ident "let")
>                       pIdent)
>                       (pseq (pmap (const id) $ lit $ Symbol '=')
>                       parserE))
>                       (pseq (pmap (const id) $ lit $ Ident "in")
>                       parserE)) $
>            palt (pmap V $ msatisfy mIdentToken) $
>            palt (pmap N $ msatisfy mNumberToken) $
>            pseq (pmap (const id) $ lit $ Symbol '(')
>                 (pseq (pmap const $ parserE) (lit $ Symbol ')'))

> build_term :: E -> [E] -> E
> build_term t [] = t
> build_term t (t1:ts) = build_term (A t t1) ts

t0 t1 t2 t3 t4
-->
(A t0 t1) t2 t3 t4
((A t0 t1) t2) t3 t4
(A (A (A t0 t1) t2) t3) t4
(A (A (A (A t0 t1) t2) t3) t4)


* BREAK!

Parsers have rich structure ...
- many concepts from category theory can be mapped to programming concepts
- parsing fits many of these concepts





* Parsing is a functor

class Functor f where
  fmap :: (a -> b) -> (f a -> f b)

here:
f |-> Parser t
f :: * -> *  is a type constructor

Parser t a :: * is a type
Parser t :: * -> *  is a type constructor

Parser :: * -> * -> *

** for parsing: fmap = pmap

> instance Functor (Parser token) where
>   fmap = pmap





* Parsing is an Applicative

Recall:

class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

** applicative laws
- Identity
pure id <*> v == v
- composition
((pure (.) <*> u) <*> v) <*> w = u <*> (v <*> w)
- homomorphism
pure f <*> pure x = pure (f x)
- interchange
u <*> pure y = pure ($ y) <*> u

> instance Applicative (Parser token) where
>   pure = succeed
>   (<*>) = pseq


** further derived members of Applicative
(*>) :: f a -> f b -> f b
fa *> fb = fmap (const id) fa <*> fb

(<*) :: f a -> f b -> f a
fa <* fb = fmap const fa <*> fb







* Parsing is applicative with an alternative

> instance Alternative (Parser token) where
>   empty = pempty
>   (<|>) = palt










can use the applicative notation for parsers:
to parse
   T -> "let" Ident "=" E "in" E 
   T -> Ident
   T -> Number
   T -> '(' E ')'

> parseT = 
>   pure L <* keyword "let"
>          <*> ident
>          <* symbol '='
>          <*> parseE
>          <* keyword "in"
>          <*> parseE
>   <|>
>   pure V <*> pSatisfy mIdentToken
>   <|>
>   pure N <*> pSatisfy mNumberToken
>   <|>
>   symbol '(' *> parseE <* symbol ')'

> parseE' :: Parser Token [E]
> parseE' =
>   pure (:) <* symbol '+' <*> parseT <*> parseE'
>   <|>
>   pure []

> parseE :: Parser Token E
> parseE =
>   pure build_term <*> parseT <*> parseE'

> keyword = lit . Ident
> symbol = lit . Symbol
> ident = pIdent
> pSatisfy = msatisfy
> 






--------------------------------------------------------------------------------
** Parsing is a monad

instance Monad (Parser t) where
  return x = undefined
  ma >>= f = undefined









** recall the monad laws
*** return x >>= f  ==  f x
*** ma >>= return   ==  ma        --  lhs:  ma >>= \x -> return x
*** (ma >>= \x -> mb) >>= f  == ma >>= \x -> (mb >>= f)    --  x not in free(f)

** list monad:
instance Monad [] where
  return x = [x]
  ma >>= f = concat $ map f ma

-- ma       :: [a]
-- f        :: a -> [b]
-- ma >>= f :: [b]

Monad laws for the list monad:
(1) return x >>= f  ==
    [x] >>= f ==
    concat $ map f [x] ==
    concat $ [f x] ==
    f x
(2) [x1, x2, ...] >>= return   ==
    concat $ map (\x -> [x]) [x1, x2, ...] ==
    concat $ [[x1], [x2], ...] ==
    [x1, x2, ...]
(3) ...

> instance Monad (Parser token) where
>   return x = (succeed x)
>   (Parser p) >>= f =
>     Parser (\ts -> concatMap g $ p ts)
>     where g (a, ts) = runParser (f a) ts


ma :: Parser' t a
p  :: Parser t a == [t] -> [(a, [t])]
f  :: a -> Parser' t b
pmap f p :: Parser t (Parser' t b)
         == [t] -> [(Parser' t b, [t])]
.. :: Parser' t b

fulfills the monad laws ...

can use the do notation for parsers:
to parse
 T -> "let" Ident "=" E "in" E 

do _ <- lit $ Ident "let"
   x <- pIdent
   _ <- lit $ Symbol '='
   e1 <- parserE
   _ <- lit $ Ident "in"
   e2 <- parserE
   return $ L x e1 e2

... readability is improved ...


* Parsing is a monad with summation

instance MonadPlus (Parser t) where
  mzero = undefined
  mplus m1 m2 = undefined

for lists:

instance MonadPlus [] where
  mzero = []
  mplus = (++)

> instance MonadPlus (Parser t) where
>   mzero = pempty
>   mplus p1 p2 = palt p1 p2








> ap' :: Monad m => m (a -> b) -> m a -> m b
> ap' mf ma = do
>   f <- mf
>   a <- ma
>   return $ f a






** Origin: applicative programming with effects (McBride, Paterson)


- example 3 : interpretation

> data Exp v
>   = Var v
>   | Val Int
>   | Add (Exp v) (Exp v)

> eval :: Exp v -> Env v -> Int
> eval (Var v) env = fetch v env
> eval (Val i) env = i
> eval (Add e1 e2) env = eval e1 env + eval e2 env

> eval' :: Exp v -> Env v -> Int
> eval' (Var v) = fetch v
> eval' (Val i) = const i
> eval' (Add e1 e2) = const (+) `ess` (eval' e1) `ess` (eval' e2)

> -- A reader monad
> type En v a = Env v -> a

> ess :: En v (a -> b) -> En v a -> En v b
> ess a b c = (a c) (b c)

> type Env v = v -> Int
> fetch :: v -> Env v -> Int
> fetch v env = env v

- examples share a common structure

