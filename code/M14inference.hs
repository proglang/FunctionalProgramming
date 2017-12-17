module M14inference where

import Control.Monad

type TyVar = String
type TyCon = String

data Type
  = TyVar TyVar
  | TyApp TyCon [Type]
  deriving (Show)

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
    g (x, (tvs, ty)) = (x, (tvs, applySubst subst ty))
  -- requires that dom subst \cap tvs = \emptyset

  
unify :: Monad m => Type -> Type -> m Subst
unify (TyVar x) (TyVar y)
  | x == y = return idSubst
  | x <  y = return [(x, TyVar y)]
  | otherwise = return [(y, TyVar x)]
unify (TyVar x) ty = return [(x, ty)]
unify ty (TyVar y) = return [(y, ty)]
unify ty1@(TyApp tc1 tys1) ty2@(TyApp tc2 tys2)
  | tc1 /= tc2 || length tys1 /= length tys2 = fail $ "failing to unify " ++ show ty1 ++ " with " ++ show ty2
  | otherwise  = unifyList idSubst tys1 tys2

unifyList :: Monad m => Subst -> [Type] -> [Type] -> m Subst
unifyList subst [] [] =
  return subst
unifyList subst (ty1:tys1) (ty2:tys2) =
  do subst1 <- unify (applySubst subst ty1) (applySubst subst ty2)
     unifyList subst1 tys1 tys2
unifyList _ _ _ =
  fail "tycon arity mismatch (should not happen)"

-- monad for HM type inference
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

type Scheme = ([TyVar], Type)

type Ass = [(ExVar, Scheme)]

gen :: Ass -> Type -> Scheme
gen ass t =
  undefined

infer :: Ass -> Exp -> HM (Subst, Type)
infer ass (ExVar x) =
  do (alphas, tx) <- mlookup x ass
     betas <- freshList alphas
     let genSubst = zip alphas $ map TyVar betas
     return (idSubst, applySubst genSubst tx)
infer ass (ExLam x e) =
  do beta <- fresh
     (subst, te) <- infer ((x, ([], TyVar beta)) : ass) e
     return (subst, TyApp "->" [applySubst subst (TyVar beta), te])
infer ass (ExApp f e) =
  do (subst0, tf) <- infer ass f
     (subst1, te) <- infer (applySubstAss subst0 ass) e
     beta <- fresh
     usubst <- unify (applySubst subst1 tf) (TyApp "->" [te, TyVar beta])
     return ( composeSubst usubst (composeSubst subst1 subst0)
            , applySubst usubst (TyVar beta))
infer ass (ExLet x e0 e1) =
  do (subst0, t0) <- infer ass e0
     let sigma = gen (applySubstAss subst0 ass) t0
     (subst1, t1) <- infer ((x, sigma) : ass) e1
     return (composeSubst subst1 subst0, t1)
infer ass (ExNum n) =
  return (idSubst, TyApp "Nat" [])
infer ass (ExSuc e) =
  do (subst, t) <- infer ass e
     usubst <- unify t (TyApp "Nat" [])
     return (composeSubst usubst subst, TyApp "Nat" [])

