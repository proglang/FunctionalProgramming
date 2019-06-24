module M13interpreter where

type Ident = String

data Term  = Con Integer
           | Bin Term Op Term
           | Var Ident
           | Let Ident Term Term -- let x = e1 in e2
             deriving (Eq, Show)
           
data Op    = Add | Sub | Mul | Div
             deriving (Eq, Show)


sys Add       =  (+)         
sys Sub       =  (-)
sys Mul       =  (*)         
sys Div       =  div


eval :: Monad m =>
  Term -> (Ident -> Integer) -> m Integer
eval (Con n) env =
  return n
eval (Bin t op u) env = do
  vt <- eval t env
  vu <- eval u env
  return $ sys op vt vu
eval (Var x) env =
  return $ env x
eval (Let x t u) env = do
  vt <- eval t env
  eval u (update x vt env)

update x vt env =
  \ y -> if x == y then vt else env y

-- passing environemnts
-- instance of the Reader monad

data Reader e a = Reader { exReader :: e -> a }

instance Functor (Reader e) where
  fmap g (Reader f) = Reader (g . f)

instance Applicative (Reader e) where
  pure a = Reader $ \ e -> a
  Reader ff <*> Reader xx =
    Reader $ \ e -> ff e (xx e)

instance Monad (Reader e) where
  Reader aa >>= f =
    Reader $ \e -> exReader (f (aa e)) e

ask :: Reader e e
ask = Reader id

local :: (e -> e') -> Reader e' a -> Reader e a
local f (Reader aa) = Reader $ \e -> aa (f e)



type Env = Ident -> Integer

evalR :: 
  Term -> Reader Env Integer
evalR = eval
  where
  eval (Con n) =
    return n
  eval (Bin t op u) = do
    vt <- eval t
    vu <- eval u
    return $ sys op vt vu
  eval (Var x) = do
    env <- ask
    return $ env x
  eval (Let x t u) = do
    vt <- eval t
    local (update x vt) (eval u)

runReader :: Reader e a -> e -> a
runReader (Reader aa) e = aa e


