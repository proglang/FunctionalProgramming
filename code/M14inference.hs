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

idSubst :: Subst
idSubst = []

applySubst :: Subst -> Type -> Type
applySubst subst (TyVar x) 
  | Just ty <- lookup x subst = ty
  | otherwise = TyVar x
applySubst subst (TyApp tc tys) = TyApp tc $ map (applySubst subst) tys
                           
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s0 =
  map g s0 ++ s1
  where
    g (tv, ty) = (tv, applySubst s1 ty)

applySubstAss :: Subst -> Ass -> Ass
applySubstAss subst = 
  map g
  where
    g (x, Forall tvs τ) = (x, Forall tvs (applySubst subst τ))
  -- requires that dom subst \cap tvs = \emptyset


-- unification
unify :: Monad m => Type -> Type -> m Subst
unify (TyVar x) (TyVar y)
  | x == y = return idSubst
  | x <  y = return [(x, TyVar y)]
  | otherwise = return [(y, TyVar x)]
unify (TyVar x) ty
  | x `elem` freetv ty = fail $ (x ++ " occurs in " ++ show ty)
  | otherwise = return [(x, ty)]
unify ty (TyVar y) = return [(y, ty)]
unify ty1@(TyApp tc1 tys1) ty2@(TyApp tc2 tys2)
  | tc1 /= tc2 || length tys1 /= length tys2 = fail $ "failing to unify " ++ show ty1 ++ " with " ++ show ty2
  | otherwise  = unifyList idSubst tys1 tys2

unifyList :: Monad m => Subst -> [Type] -> [Type] -> m Subst
unifyList subst [] [] =
  return subst
unifyList subst (ty1:tys1) (ty2:tys2) =
  do subst1 <- unify (applySubst subst ty1) (applySubst subst ty2)
     unifyList (composeSubst subst1 subst) tys1 tys2
unifyList _ _ _ =
  fail "tycon arity mismatch (should not happen)"

data UError a
  = ULeft String
  | URight a

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
  ua <*> ub = case ua of
                ULeft s -> ULeft s
                URight f ->
                  case ub of
                    ULeft s -> ULeft s
                    URight b -> URight (f b)







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

mlookup :: (Monad m, Eq a) => a -> [(a, b)] -> m b
mlookup x xys =
  case lookup x xys of
    Nothing -> fail "mlookup failed"
    Just y -> return y

fresh :: HM TyVar
fresh =
  HM (\s -> 
        let s' = s { count = count s + 1 }
            tv = 't' : show (count s')
        in  Right (tv, s'))

freshList :: [x] -> HM [TyVar]
freshList = mapM (const fresh)

-- mini-ml

type ExVar = String

data Exp
  = ExVar ExVar
  | ExLam ExVar Exp
  | ExApp Exp Exp
  | ExLet ExVar Exp Exp
  | ExNum Integer
  | ExSuc Exp

data Scheme = Forall [TyVar] Type

type Ass = [(ExVar, Scheme)]

freetv :: Type -> [TyVar]
freetv (TyVar tv) = [tv]
freetv (TyApp tc tys) = foldr union [] $ map freetv tys

freetvAss :: Ass -> [TyVar]
freetvAss = foldr union [] . map freetvAssEntry

freetvAssEntry :: (ExVar, Scheme) -> [TyVar]
freetvAssEntry = freetvScheme . snd

freetvScheme :: Scheme -> [TyVar]
freetvScheme (Forall gtvs ty) = freetv ty \\ gtvs

gen :: Ass -> Type -> Scheme
gen ass ty =
  let fvt = freetv ty
      fva = freetvAss ass
  in  Forall (fvt \\ fva) ty






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
