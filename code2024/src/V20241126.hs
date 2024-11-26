module V20241126 where


data Term =
  Con Integer
  | Bin Term Op Term

data Op = Add | Sub | Mul | Div

eval :: Monad m => Term -> m Integer
eval (Con n) = return n
eval (Bin l o r) = do
  v <- eval l
  w <- eval r
  return (sys o v w)

sys :: Op -> Integer -> Integer -> Integer
sys Add = (+)
sys Sub = (-)
sys Mul = (*)
sys Div = div

