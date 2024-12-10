module V20241210 where

newtype Comp f g a = Comp (f (g a))

instance (Functor f, Functor g) => Functor (Comp f g) where
  -- fmap :: (a -> b) -> Comp f g a -> Comp f g b
  fmap h (Comp fga)
     = Comp $ fmap{-f-} (fmap{-g-} h) fga
       --                ^-> :: g a -> g b

-- functor laws hold!

instance (Applicative f, Applicative g) =>
  Applicative (Comp f g) where
  pure x = Comp $ pure (pure x)
  Comp fga_b <*> Comp fga = Comp $ fmap (<*>) fga_b <*> fga
  -- fga_b :: f (g (a -> b))
  -- fga   :: f (g a)
  -- fmap (<*>) fga_b :: f (g a -> g b)
  --      ^-> :: g (a -> b) -> (g a -> g b)
  -- fmap (<*>) fga_b <*> fga :: f (g b)

data ST s a = ST (s -> (s, a))

instance Functor (ST s) where
  fmap h (ST g) = ST (fmap h . g)

instance Applicative (ST s) where
  pure a = ST (, a)
  ST fab <*> ST fa = ST $ \s -> let (s', f) = fab s in
                                fmap f $ fa s'

instance Monad (ST s) where
  ST fa >>= h = ST (\s -> let (s', a) = fa s in
                          let ST ha = h a in
                            ha s')

type STM s a = Comp Maybe (ST s) a

nuke :: Maybe a -> Maybe a
nuke _ = Nothing

reset :: ST Int a -> ST Int a
reset (ST g) = ST (g . const 0)

use_nuke :: STM s a -> STM s a
use_nuke (Comp stm) = Comp (nuke stm)

use_reset :: STM Int a -> STM Int a
use_reset (Comp stm) = Comp (fmap reset stm)

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance Monad m => Functor (StateT s m) where
  fmap h (StateT g) = StateT $ \s -> do
    (a, s') <- g s
    return (h a, s')

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> return (a, s)
  (<*>) = undefined

instance (Monad m) => Monad (StateT s m) where
  m >>= f = StateT $ \s -> do
    (a, s') <- runStateT m s
    runStateT (f a) s'

class MonadTrans t where
  lift :: (Monad m) => m a -> t m a

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do { a <- ma ; return (a, s) }

