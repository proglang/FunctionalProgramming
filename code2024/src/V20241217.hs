{-# LANGUAGE GADTs #-}
module V20241217 where

data Term a where
 I :: Integer -> Term Integer
 D :: Double -> Term Double
 B :: Bool -> Term Bool
 Add :: (Num a) => Term a -> Term a -> Term a
 Eql :: (Eq a) => Term a -> Term a -> Term Bool
-- deriving (Eq, Show)

eval :: Term a -> a
eval (I i) = i
eval (B b) = b
eval (D d) = d
eval (Add t1 t2) = eval t1 + eval t2
eval (Eql t1 t2) = eval t1 == eval t2

























































data env :- a where
  App :: env :- (a -> b)
      -> env :- a
      -> env :- b
  Lam :: (a, env) :- b
      -> env :- (a -> b)
  Var :: env :> a
      -> env :- a
  Con :: Integer
      -> env :- Integer
  Suc :: env :- Integer
      -> env :- Integer

data env :> a where
  Z :: (a, env) :> a
  S :: env :> a -> (b, env) :> a

flookup :: (env :> a) -> env -> a
flookup Z     (a, env) = a
flookup (S n) (b, env) = flookup n env

feval :: (env :- a) -> env -> a
feval (App t1 t2) env =
  feval t1 env  {- :: a -> b -} $
  feval t2 env  {- :: a      -}
feval (Lam t) env = \a -> feval t (a, env)
feval (Var v) env = flookup v env
feval (Con i) env = i
feval (Suc t) env = feval t env + 1

t1 :: env :- (a -> a)
t1 =  Lam (Var Z)

fsuc :: env :- (Integer -> Integer)
-- \x -> Suc x
fsuc = Lam (Suc (Var Z))

t2 :: env :- ((a -> a) -> (a -> a))
-- t2 = \f x -> f (f x)
t2 = Lam (Lam (App (Var (S Z)) (App (Var (S Z)) (Var Z))))

--- numbers as types

data TZero where

data TSucc a where

data Vector n b where
  Vempty :: Vector TZero b
  Vcons  :: b -> Vector n b -> Vector (TSucc n) b

vhead :: Vector (TSucc n) b -> b
vhead (Vcons x _) = x

