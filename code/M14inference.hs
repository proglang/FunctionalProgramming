module M14inference where

import Control.Monad
import Data.List (union, (\\))

type TyVar = String
type TyCon = String

data Type
  = TyVar TyVar
  | TyApp TyCon [Type]
  deriving (Show)

-- example types
tyNat = TyApp "Nat" []
tyFun τ1 τ2 = TyApp "->" [τ1, τ2]
tyList τ = TyApp "[]" [τ]

tyAlpha = TyVar "a"
tyBeta  = TyVar "b"

-- substitutions represented as lists of pairs
type Subst = [(TyVar, Type)]

-- domain of substitution
domSubst :: Subst -> [TyVar]
domSubst = map fst

-- identity substitution
idSubst :: Subst
idSubst = []

-- apply a substitution to a type
applySubst :: Subst -> Type -> Type
applySubst subst (TyVar x) 
  | Just ty <- lookup x subst = ty
  | otherwise = TyVar x
applySubst subst (TyApp tc tys) = TyApp tc $ map (applySubst subst) tys

-- compose substitutions
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s0 =
  map g s0 ++ filter p s1
  where
    g (tv, ty) = (tv, applySubst s1 ty)
    p (tv, ty) = not $ tv `elem` domS0
    domS0 = domSubst s0

-- apply a substitution to a type scheme
applySubstScheme :: Subst -> Scheme -> Scheme
applySubstScheme subst (Forall tvs τ) = 
  Forall tvs (applySubst subst' τ)
  -- requires that dom subst \cap tvs = \emptyset
  where
    subst' = filter p subst
    p (α, τ) = not (α `elem` tvs)

-- apply a substitution to a type assumption
applySubstAss :: Subst -> Ass -> Ass
applySubstAss subst = 
  map $ \ (x, σ) -> (x, applySubstScheme subst σ)

-- unification
unify :: Monad m => Type -> Type -> m Subst
unify (TyVar x) (TyVar y)
  | x == y = return idSubst
  | x <  y = return [(x, TyVar y)]
  | otherwise = return [(y, TyVar x)]
unify (TyVar x) ty
  | x `elem` freetv ty =
      fail $ (x ++ " occurs in " ++ show ty)
  | otherwise =
      return [(x, ty)]
unify ty (TyVar y) = return [(y, ty)]
unify ty1@(TyApp tc1 tys1) ty2@(TyApp tc2 tys2)
  | tc1 /= tc2 || length tys1 /= length tys2 =
      fail $ "failing to unify " ++ show ty1 ++ " with " ++ show ty2
  | otherwise  =
      unifyList idSubst tys1 tys2

unifyList :: Monad m => Subst -> [Type] -> [Type] -> m Subst
unifyList subst [] [] =
  return subst
unifyList subst (ty1:tys1) (ty2:tys2) =
  do subst1 <- unify (applySubst subst ty1) (applySubst subst ty2)
     unifyList (composeSubst subst1 subst) tys1 tys2
unifyList _ _ _ =
  fail "tycon arity mismatch (should not happen)"

-- monad for reporting error messages
-- Haskell forces us to define instances of Functor and Applicative
data UError a
  = ULeft String                -- error message
  | URight a                    -- returned result

instance Monad UError where
  return a = URight a
  m >>= f  = case m of
               ULeft s -> ULeft s
               URight a -> f a
  fail s = ULeft s

instance Functor UError where
  fmap f ua = case ua of
                ULeft s -> ULeft s
                URight a -> URight (f a)

instance Applicative UError where
  pure a = URight a
  uf <*> ub = case uf of
                ULeft s -> ULeft s
                URight f ->
                  case ub of
                    ULeft s -> ULeft s
                    URight b -> URight (f b)

-- examples for unification
uex1, uex2, uex3, uex4, uex5, uex6, uex7, uex8 :: Monad m => m Subst
uex1 = unify tyAlpha tyAlpha
uex2 = unify tyAlpha tyBeta
uex3 = unify tyAlpha tyNat
uex4 = unify tyNat tyBeta
uex5 = unify tyNat (tyFun tyBeta tyBeta) -- error
uex6 = unify tyBeta (tyFun tyBeta tyBeta) -- error
uex7 = unify (tyFun tyAlpha (tyFun tyBeta tyAlpha)) (tyFun (TyVar "γ") (TyVar "δ"))
uex8 = unify (tyFun tyAlpha             tyBeta)
             (tyFun (tyFun tyNat tyNat) tyAlpha)








-- Monad for HM type inference
-- combination of two monads:
-- * fresh variable generation: state monad
-- * error messages: Either monad

type HMError = String
data HMState = HMState { count :: Int }

data HM a 
  = HM { exHM :: HMState -> Either HMError (a, HMState) }

instance Functor HM where
  fmap f hma =
    HM (\s -> case exHM hma s of
                Left msg -> Left msg
                Right (a, s') -> Right (f a, s')
       )

instance Applicative HM where
  pure x = 
    HM (\s -> Right (x, s))
  ax <*> ay =
    HM (\s -> case exHM ax s of
                Left msg -> Left msg
                Right (x, s') ->
                  case exHM ay s' of
                    Left msg -> Left msg
                    Right (y, s'') ->
                      Right (x y, s'')
       )

instance Monad HM where
  return x =
    HM (\s -> Right (x, s))
  m >>= f =
    HM (\s -> case exHM m s of
                Left msg -> Left msg
                Right (x, s') -> exHM (f x) s'
       )
  fail msg =
    HM (\s -> Left msg)

-- run the HM monad
runHM :: HM a -> Either String a
runHM hma =
  case exHM hma HMState { count = 0 } of
    Left m -> Left m
    Right (x, s) -> Right x

-- lookup a value in an association list
mlookup :: (Monad m, Eq a) => a -> [(a, b)] -> m b
mlookup x xys =
  case lookup x xys of
    Nothing -> fail "mlookup failed"
    Just y -> return y

-- generate a fresh type variable
fresh :: HM TyVar
fresh =
  HM (\s -> 
        let s' = s { count = count s + 1 }
            tv = 'a' : show (count s')
        in  Right (tv, s'))

-- generate a list of fresh type variables
freshList :: [x] -> HM [TyVar]
freshList = mapM (const fresh)

-- mini-ml syntax
type ExVar = String

data Exp
  = ExVar ExVar
  | ExLam ExVar Exp
  | ExApp Exp Exp
  | ExLet ExVar Exp Exp
  | ExNum Integer
  | ExSuc Exp

-- type schemes
data Scheme = Forall [TyVar] Type

-- type assumptions
type Ass = [(ExVar, Scheme)]

-- free variables in a type
freetv :: Type -> [TyVar]
freetv (TyVar tv) = [tv]
freetv (TyApp tc tys) = foldr union [] $ map freetv tys

-- fv in a type assumption
freetvAss :: Ass -> [TyVar]
freetvAss = foldr union [] . map (freetvScheme . snd)

-- fv in a type scheme
freetvScheme :: Scheme -> [TyVar]
freetvScheme (Forall gtvs ty) = freetv ty \\ gtvs

-- the gen function
gen :: Ass -> Type -> Scheme
gen ass ty =
  let fvt = freetv ty
      fva = freetvAss ass
  in  Forall (fvt \\ fva) ty



-- algorithm W
infer :: Ass -> Exp -> HM (Subst, Type)
infer ass (ExVar x) =
  do Forall αs tx <- mlookup x ass
     βs <- freshList αs
     let genSubst = zip αs $ map TyVar βs
     return (idSubst, applySubst genSubst tx)
infer ass (ExLam x e) =
  do β <- fresh
     (subst, τ) <- infer ((x, Forall [] (TyVar β)) : ass) e
     return (subst, TyApp "->" [applySubst subst (TyVar β), τ])
infer ass (ExApp e0 e1) =
  do (subst0, τ0) <- infer ass e0
     (subst1, τ1) <- infer (applySubstAss subst0 ass) e1
     β <- fresh
     usubst <- unify (applySubst subst1 τ0) (TyApp "->" [τ1, TyVar β])
     return ( composeSubst usubst (composeSubst subst1 subst0)
            , applySubst usubst (TyVar β))
infer ass (ExLet x e0 e1) =
  do (subst0, τ0) <- infer ass e0
     let σ = gen (applySubstAss subst0 ass) τ0
     (subst1, τ1) <- infer ((x, σ) : ass) e1
     return (composeSubst subst1 subst0, τ1)
infer ass (ExNum n) =
  return (idSubst, TyApp "Nat" [])
infer ass (ExSuc e) =
  do (subst, τ) <- infer ass e
     usubst <- unify τ (TyApp "Nat" [])
     return (composeSubst usubst subst, TyApp "Nat" [])

-- examples
iex1 = ExLam "x" (ExVar "x")
iex2 = ExLam "x" (ExLam "y" (ExVar "x"))
iex3 = ExLet "i" iex1 (ExApp (ExVar "i") (ExNum 42))
iex4 = ExLet "i" iex1 (ExApp (ExVar "i") (ExVar "i"))

ass1 = [("map", Forall ["a", "b"] (tyFun (tyFun tyAlpha tyBeta) (tyFun (TyApp "[]" [tyAlpha]) (TyApp "[]" [tyBeta]))))
       ,("p", Forall [] (tyFun (tyNat) (TyApp "Bool" [])))]







-- try to define HM on top of UError

data HM' a
  = HM' { exHM' :: HMState -> UError (a, HMState) }

instance Functor HM' where
  fmap f hma =
    HM' (\s -> fmap (f # id) $ exHM' hma s)

(#) :: (a1 -> b1) -> (a2 -> b2) -> (a1, a2) -> (b1, b2)
(f1 # f2) (a1, a2) = (f1 a1, f2 a2)

instance Applicative HM' where
  pure x =
    HM' (\s -> pure (x, s))
  ax <*> ay =
    undefined

instance Monad HM' where
  return x =
    HM' (\s -> return (x, s))
  m >>= f =
    HM' (\s -> exHM' m s >>= \(x, s') -> exHM' (f x) s')

