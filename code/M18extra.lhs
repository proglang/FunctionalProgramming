> module M18extra where
> import Data.Bifunctor

> type Bin = [Bit]
> data Bit = O | I deriving (Show, Enum)

> unbits :: Enum a => Int -> Bin -> (a, Bin)
> unbits n bs = first toEnum $ unbitsInt n bs

> unbitsInt :: Int -> Bin -> (Int, Bin)
> unbitsInt n (b:bs) | n > 0 = first (fromEnum b * 2^(n-1) +) $ unbitsInt (n-1) bs
> unbitsInt n bs     | n == 0 = (0, bs)


