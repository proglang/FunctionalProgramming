data Suit =
    King
  | Ace
  | Queen
  | Jack
  | Numeric Integer
    deriving (Show, Read)

instance Eq Suit where
    King == King = True
    Ace == Ace = True
    Queen == Queen = True
    Jack == Jack = True
    Numeric i == Numeric i2 = i == i2
    _ == _ = False

data NumPlus a = Integer a | PlusInf | MinusInf

instance Num a => Num (NumPlus a) where
    (Integer i) + (Integer i') = Integer (i + i')
    PlusInf + _ = PlusInf
    _ + PlusInf = PlusInf
    MinusInf + _ = MinusInf
    _ + MinusInf = MinusInf

    (Integer i) * (Integer i') = Integer (i * i')
    abs (Integer i) = Integer (abs i)
    signum (Integer i) = Integer (signum i)
    fromInteger i = Integer (fromInteger i)

    negate (Integer i) = Integer (negate i)
    negate PlusInf = MinusInf
    negate MinusInf = PlusInf

-- f x = read (show x)
-- g x = show (read x)

class Stack s where
  push :: s a -> a -> s a
  pop  :: s a -> s a
  top  :: s a -> a
  init :: s a

instance Stack [] where
  push = flip (:)
  pop  = tail
  top  = head
  init = []

