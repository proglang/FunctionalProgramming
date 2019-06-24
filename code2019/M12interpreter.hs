{-# LANGUAGE DeriveFunctor #-}
module M12interpreter where

data Term  = Con Integer
           | Bin Term Op Term  
             deriving (Eq, Show)
           
data Op    = Add | Sub | Mul | Div
             deriving (Eq, Show)

eval              :: Term -> Integer
eval (Con n)      =  n
eval (Bin t op u) =  sys op (eval t) (eval u)

sys Add       =  (+)         
sys Sub       =  (-)
sys Mul       =  (*)         
sys Div       =  div






-- some example terms







evalErr :: Term -> Exception Integer
evalErr = eval where
  eval (Con n)      = Return n
  eval (Bin t op u) = case eval t of
                        Raise  s -> Raise s
                        Return v -> case eval u of
                          Raise  s -> Raise s
                          Return w ->
                            if (op == Div && w == 0)
                            then  
                              Raise "div by zero"
                            else                                  
                              Return (sys op v w)











newtype Trace a = Trace (a, String)
  deriving (Show)

evalTrace :: Term -> Trace Integer
evalTrace = eval where
  eval e@(Con n)      = Trace (n, trace e n)
  eval e@(Bin t op u) = 
     let Trace (v, x) = eval t in
     let Trace (w, y) = eval u in
     let r = sys op v w in
     Trace (r, x ++ y ++ trace e r)

trace t n = "eval (" ++ show t ++ ") = "
                ++ show n ++ "\n"
















newtype Count a = Count { exCount :: Int -> (a, Int) }

evalCount :: Term -> Count Integer
evalCount = eval where
  eval (Con n)      = Count $ \i -> (n, i)
  eval (Bin t op u) = Count $ \i -> let (v, j) = exCount (eval t) i in    
                                    let (w, k) = exCount (eval u) j in
                                    (sys op v w, k + 1)









ex1 = Con 42
ex2 = Bin (Con 17) Add (Con 4)
ex3 = Bin ex2 Div (Con 0)
ex4 = Bin ex2 Mul ex2




evalMonad              :: Monad m => Term -> m Integer
evalMonad = eval where
  eval (Con n)      =  return n
  eval (Bin t op u) =  eval t  >>= \v ->
                       eval u  >>= \w ->
                       return (sys op v w)














-- identity monad
newtype Id a = Id a

instance Functor Id where
  -- fmap :: (a -> b) -> Id a -> Id b
  fmap g (Id a) = Id (g a)



instance Applicative Id where
  pure x = Id x
  -- Id (a -> b) -> Id a -> Id b
  Id f <*> Id x = Id (f x)



instance Monad Id where
  return x = Id x
  -- Id a -> (a -> Id b) -> Id b
  Id x >>= f  = f x









-- modeling exceptions
data Exception a =  Raise  String
                 |  Return a
  deriving (Show)

-- exception monad
instance Functor Exception where
  fmap g (Raise s) = Raise s
  fmap g (Return a) = Return (g a)

instance Applicative Exception where
  pure x = Return x
  -- (<*>) :: Exception (a -> b) -> Exception a -> Exception b
  Return g <*> Return a = Return (g a)
  Raise s  <*> _        = Raise s
  _        <*> Raise s  = Raise s

{- equivalent definition using the fact Functor Exception -}
{- unreachable -}
  Return g <*> exx      = fmap g exx
  Raise s  <*> _        = Raise s

instance Monad Exception where
  -- (>>=) :: Exception a -> (a -> Exception b) -> Exception b
  Return a >>= f = f a
  Raise s >>= f = Raise s
  fail s = Raise s

raise :: String -> Exception a
raise s = Raise s

-- ma >> mb = ma >>= const mb

















evalMExc             :: Term -> Exception Integer
evalMExc = eval where
  eval (Con n)      = return n
  eval (Bin t op u) = eval t  >>= \v ->
                      eval u  >>= \w ->
                      if (op == Div && w == 0) 
                       then fail "div by zero"
                      else return (sys op v w)












-- tracing monad
instance Functor Trace where
  fmap g (Trace (a, out)) = Trace (g a, out)

instance Applicative Trace where
  pure a = Trace (a, "")
  -- g :: a -> b, a :: a
  Trace (g, out) <*> Trace (a, out') = Trace (g a, out ++ out')

instance Monad Trace where
  Trace (a, out) >>= g = let Trace (b, out') = g a
                         in  Trace (b, out ++ out')

output :: String -> Trace ()
output s = Trace ((), s)





-- monadic interpreter with tracing
evalMTrace :: Term -> Trace Integer
evalMTrace = eval where
  eval e@(Con n) = output (trace e n) >>
                   return n
  eval e@(Bin t op u) = eval t >>= \v ->
                        eval u >>= \w ->
                        let r = sys op v w in
                        output (trace e r) >>
                        return r






-- counting monad
instance Functor Count where
  -- fmap :: (a -> b) -> Count a -> Count b
  fmap g (Count f) = Count $ \ i -> let (a, i') = f i in (g a, i')

instance Applicative Count where
  pure x = Count $ \ i -> (x, i)
  Count cf <*> Count cx = Count $ \ i -> let (f, j) = cf i in
                                         let (x, k) = cx j in
                                         (f x, k)

instance Monad Count where
  -- f :: a -> Count b
  Count cx >>= f = Count $ \ i -> let (x, j) = cx i in
                                  exCount (f x) j


incr :: Count ()
incr = Count $ \ i -> ((), i+1)


runCount :: Count a -> Int -> (a, Int)
runCount (Count f) x = f x


-- monadic interpreter with counting

evalMCount :: Term -> Count Integer
evalMCount = eval where
  eval (Con n)      =  return n
  eval (Bin t op u) =  eval t >>= \v ->
                       eval u >>= \w ->
                       incr   >>
                       return (sys op v w)


-- * excursion on kinding of type constructors

-- kind of Count
-- Count :: * -> *
-- Id :: * -> *
-- [] :: * -> *

data ST s a = ST { exST :: s -> (a, s) }

-- kind of ST
-- ST :: * -> * -> *

-- ST Int :: * -> *
-- ST Int () :: *

-- (,) :: * -> * -> *
-- (,) Int :: * -> *

-- (,,) :: * -> * -> * -> *

-- * a different interface to monads

-- Haskell : functions of type a -> b
-- Monad   : functions of type (a -> m b)

-- if all functions have type of shape (a -> m b)
-- examples: 2nd argument of (>>=)
--           return :: (a -> m a)

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)

mcomp :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
mcomp f g = \ a -> g a >>= \ b -> f b
--          = \ a -> g a >>= f

{-
return `mcomp` g == \ a -> g a >>= return == \ a -> g a == g

f `mcomp` return == \ a -> return a >>= f == \ a -> f a == f

(f `mcomp` g) `mcomp` h == f `mcomp` (g `mcomp` h)
-}

-- * the list monad

{-
instance Monad [] where
  return x = [x]
  -- (>>=) :: [a] -> (a -> [b]) -> [b]
  la >>= f = concatMap f la
-}
