module M14compose where

newtype Comp f g a = Comp { exComp :: (f (g a)) }

instance (Functor f, Functor g) => Functor (Comp f g) where
  -- fmap :: (a -> b) -> Comp f g a -> Comp f g b
  -- fga :: f (g a)
  fmap h (Comp fga) = Comp $ fmap (fmap h) fga

-- fmap (h1 . h2) fga
-- == fmap_f (fmap_g (h1 . h2)) fga
-- == fmap_f (fmap_g h1 . fmap_g h2)) fga
-- == (fmap_f (fmap_g h1) . fmap_f (fmap_g h2)) fga
-- == (fmap h1 . fmap h2) fga

cv = Comp [Just 14, Just 2, Nothing, Just 4711]

-- (>>=) :: m a -> (a -> m b) -> m b

instance (Applicative f, Applicative g) => Applicative (Comp f g) where
  -- pure :: a -> f (g a)
  -- for g: pure :: a -> g a
  -- for f: pure :: (g a) -> f (g a)
  pure a = Comp $ pure (pure a)
  Comp fff <*> Comp aaa = Comp $
    -- fff :: f (g (a -> b))
    -- aaa :: f (g a)
    -- result :: f (g b)
    fmap (<*>) fff -- :: f (g a -> g b)
      <*> aaa

cfff = Comp [Just (+1), Nothing, Just (*2)]
vaaa = Comp [Nothing, Just 42, Just 17]

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
instance Functor (MaybeT m) where
  fmap = undefined
instance Applicative (MaybeT m) where
  pure = undefined
  (<*>) = undefined

instance (Monad m) => Monad (MaybeT m) where
  return a = MaybeT $ return (return a)
  (MaybeT mmx) >>= f = MaybeT $ do
    mx <- mmx
    case mx of
      Nothing -> return Nothing
      Just x  -> runMaybeT (f x)
  fail str = MaybeT $ return Nothing

{-
instance MonadTrans MaybeT where
  lift mx = MaybeT $ mx >>= (return . Just)
-}

newtype ReaderT m r a = ReaderT { runReaderT :: r -> m a }
instance Functor (ReaderT m r) where
  fmap = undefined
instance Applicative (ReaderT m r) where
  pure = undefined
  (<*>) = undefined
instance (Monad m) => Monad (ReaderT m r) where
  return a = ReaderT $ const (return a)
  (ReaderT rma) >>= f =
    ReaderT $ \ r -> do
      a <- rma r
      runReaderT (f a) r
  fail str = ReaderT $ const (fail str)

ask :: Monad m => ReaderT m r r
ask = ReaderT $ \r -> return r

-- StateT s Maybe a === s -> Maybe (a, s)
-- MaybeT (ST s) a === s -> (Maybe a, s)

data Term = Var String | Con Integer | Bin Term Op Term
  deriving (Eq, Show)

data Op = Add | Sub | Mul | Div
  deriving (Eq, Show)

newtype Identity a = Identity{ runIdentity :: a }

instance Functor Identity where
  fmap h (Identity a) = Identity (h a)
instance Applicative Identity where
  pure a = Identity a
  Identity f <*> Identity a = Identity (f a)
instance Monad Identity where
  return = Identity
  Identity a >>= f = f a

type M = ReaderT (MaybeT Identity) [(String, Integer)]

applyOp :: Op -> Integer -> Integer -> M Integer
applyOp Add x y = return (x + y)
applyOp Div x y | y == 0 = fail "division by zero"
                | otherwise = return ( x `div` y )

eval :: Term -> M Integer
eval (Var x) = do
  env <- ask
  case lookup x env of
    Just i -> return i
    Nothing -> fail "unknown variable"
eval (Con i) = return i
eval (Bin l o r) = do
  vl <- eval l
  vr <- eval r
  applyOp o vl vr

runM t env = runIdentity (runMaybeT (runReaderT (eval t) env))
