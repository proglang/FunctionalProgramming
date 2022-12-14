{-# LANGUAGE QualifiedDo #-}
module V11 where

-- import SimplePrelude as S
-- import Prelude ()

  
ap :: Monad m => m (a -> b) -> m a -> m b
ap mfab ma = do
  f <- mfab
  a <- ma
  return (f a)
