module M14compose where

newtype Comp f g a = Comp { exComp :: (f (g a)) }

instance (Functor f, Functor g) => Functor (Comp f g) where
  -- fmap :: (a -> b) -> Comp f g a -> Comp f g b
  -- fga :: f (g a)
  fmap h (Comp fga) = Comp $ fmap (fmap h) fga


cv = Comp [Just 14, Just 2, Nothing, Just 4711]

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

{-
instance MonadTrans MaybeT where
  lift mx = MaybeT $ mx >>= (return . Just)
-}

-- StateT s Maybe a === s -> Maybe (a, s)
-- MaybeT (ST s) a === s -> (Maybe a, s)
