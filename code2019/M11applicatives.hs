module M11applicates where


sequence' :: [IO a] -> IO [a]
sequence' [] = return []
sequence' (io: ios) = return (:) `ap` io `ap` sequence' ios


ap :: IO (a -> b) -> IO a -> IO b
ap iof iox = do
  f <- iof
  x <- iox
  return (f x)

transpose :: [[a]] -> [[a]]
transpose [] = repeat []
transpose (xs:xss) = zipWith (:) xs (transpose xss)

data Exp v
  = Var v | Val Int | Add (Exp v) (Exp v)

eval :: Exp v -> Env v -> Int
eval (Var v) env = fetch v env
eval (Val i) env = const i env -- == i
eval (Add e1 e2) env = eval e1 env + eval e2 env

type Env v = v -> Int

fetch :: v -> Env v -> Int
fetch v env = env v


eval' :: Exp v -> Env v -> Int
eval' (Var v) = fetch v 
eval' (Val i) = const i
eval' (Add e1 e2) = const (+) `ess` eval' e1 `ess` eval' e2

ess :: (Env v -> (a -> b))
    -> (Env v -> a)
    -> (Env v -> b)
ess fab fa env = (fab env) (fa env)

exp1 = Add (Val 17) (Var "x")
env1 = \"x" -> 4 :: Int

data EnvV = EnvV
data A = A
data B = B

ess' :: (EnvV -> (A -> B))
     -> (EnvV -> A)
     -> (EnvV -> B)
ess' = undefined
