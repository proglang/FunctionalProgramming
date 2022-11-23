-- Copyright 2022 University of Freiburg
-- Janek Spaderna <janek.spaderna@pluto.uni-freiburg.de>
--
-- Add these two import statements at the top of your file
--
--    import SimplePrelude as S
--    import Prelude ()
--
-- To write `do` blocks which use the operators from SimplePrelude instead of
-- Prelude enable the QualifiedDo extension and write your `do` blocks as
-- `SimplePrelude.do` (or import it `as XYZ` and use `XYZ.do`):
--
--    {-# LANGUAGE QualifiedDo #-}
--
--    liftM f ma = SimplePrelude.do
--      a <- ma
--      return (f a)
--
--
{-# LANGUAGE ImportQualifiedPost #-}

module SimplePrelude
  ( -- * Re-exports
    module Prelude,

    -- * @Functor@
    Functor (..),
    (<$>),

    -- * @Applicative@
    Applicative (..),
    liftA2,
    (<*),
    (*>),

    -- * @Monad@
    Monad (..),
    (>>),
    (=<<),

    -- * @MonadFail@
    MonadFail (..),
  )
where

import Prelude hiding
  ( Applicative (..),
    Functor (..),
    Monad (..),
    MonadFail (..),
    (<$>),
    (=<<),
  )
import Prelude qualified as P

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

class Monad m => MonadFail m where
  fail :: String -> m a

-------------------------------------------------------------------------------
-- Functor based Prelude functions

infixl 4 <$>

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

-------------------------------------------------------------------------------
-- Applicative based Prelude functions

infixl 4 <*>, <*, *>

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 ab2c fa fb = pure ab2c <*> fa <*> fb

(<*) :: Applicative f => f a -> f b -> f a
(<*) = liftA2 const

(*>) :: Applicative f => f a -> f b -> f b
(*>) = liftA2 (const id)

-------------------------------------------------------------------------------
-- Monad based Prelude functions

infixl 1 >>=, >>

infixr 1 =<<

(>>) :: Monad m => m a -> m b -> m b
ma >> mb = ma >>= \_ -> mb

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

-------------------------------------------------------------------------------
-- IO instances

instance Functor P.IO where
  fmap = P.fmap

instance Applicative P.IO where
  pure = P.pure
  (<*>) = (P.<*>)

instance Monad P.IO where
  return = P.return
  (>>=) = (P.>>=)

instance MonadFail P.IO where
  fail = P.fail
