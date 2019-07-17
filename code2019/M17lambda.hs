
{-
-- free and bound
ex0 = \ x -> \ y -> x
-- free(ex0) = {}
-- bound (ex0) = {x, y}
ex1 = x
-- free(ex1) = {x}
-- bound (ex1) = {}
ex2 = \ x -> x
-- free(ex2) = {}, closed, combinator
-- bound (ex2) = {x}
ex3 = (\ x -> x) x
-- free (ex3) = bound (ex3) = {x}


-- substitution
-- of x |-> ne

se1 = x  |-> ne 
    = y  |-> y
    = e1 e2 |->  e1 [ x|-> ne]
                 (e2 [ x|-> ne])

(\ y -> y x) [ x |-> 42 ] = (\ y -> y 42)

(\ y -> y x) [ x |-> y ] =!= (\ y -> y y)

(\ y -> y x) [ x |-> y ] = (\ y' -> y' x) [ x |-> y ] = (\ y' -> y' y)


-- left side of computation rule:
-- redex
-- right side:
-- reductum, contractum

(\x -> (y x)) -- no beta redex

(\ x -> (y x)) M -beta-> (y M)

(\ x -> (y x)) -eta-> y

-}

{-
a lambda term M has at most one normal form

Suppose
M -b*-> N1 a normal form
M -b*-> N2 another normal form

hence N1 <-b*-> N2
by CR there is some N such that\
N1 -b*-> N
N2 -b*-> N

but N1, N2 are normal forms,
so N1 =a= N and N =a= N2
so N1 =a= N2

-}

tt = \ x y -> x
ff = \ x y -> y
iff = \ b t f -> b t f

{-

iff tt 42 57
=
(((\ b t f -> b t f) tt) 42) 57
-b->
((\ t f -> tt t f) 42) 57
-b->
(\ f -> tt 42 f) 57
-b->
tt 42 57
=
((\ x y -> x) 42) 57
-b->
(\ y -> 42) 57
-b->
42

-}

zero = \f x -> x
one  = \f x -> f x
two  = \f x -> f (f x)
three= \f x -> f (f (f x))

succ1 = \n f x -> f (n f x)
succ2 = \n f x -> n f (f x)

{-

m f x = f^m x
n f x = f^n x

(\ g -> m n g) g y =
(\ g -> n^m g) g y =
n^m g y =
g^(n^m) y

two f x = f (f x)
three f x = f (f (f x))

two three g x =
three (three g) x =
(three g (three g (three g x)))

-}

-- [0] = \f \x. x
-- if0 [0] Z S --> Z
-- if0 [n+-] Z S --> S

if0 = \n -> \z -> \s -> n (\z -> s) z
{-
if0 zero Z S
=
(\n -> \z -> \s -> n (\z -> s) z) zero Z S
-->
(\z -> \s -> zero (\z -> s) z) Z S
-->
(\s -> zero (\z -> s) Z) S
-->
(zero (\z -> S) Z)
=
((\f -> \x -> x) (\z -> S) Z)
-->
(\x -> x) Z
-->
Z
-}

pair = \x -> \y -> \f -> f x y

triple = \x y z -> \f -> f x y z

xcase = \e -> \nl -> \nr -> e nl nr

left  = \pl -> \nl nr -> nl pl
right = \pr -> \nl nr -> nr pr

-- subtraction for Church numerals
-- requirement: predecessor

xsucc = \n f x -> f (n f x)

xpred = \n -> 
  fst (n (\(_, r) -> (r, xsucc r)) (zero , zero))

church :: Int -> (a -> a) -> a -> a
church 0 = zero
church n = xsucc (church (n-1))

{-
y = \f -> (\x -> f (x x)) (\x -> f (x x))

N = Y M

N
=
(Y M)
=
((\f -> (\x -> f (x x)) (\x -> f (x x))) M)
-->
((\x -> M (x x)) (\x -> M (x x)))
-->
M ((\x -> M (x x)) (\x -> M (x x)))

<--
M ((\f -> (\x -> f (x x)) (\x -> f (x x))) M)
=
M (Y M)
=
M N


for Y we have: Y M = M (Y M)

-}

fix m = m (fix m)

-- the factorial function

-- write a non-recursive function
-- that abstracts over the recursive call

fact = 
  \fac ->
    \n -> if n == 0 then 1 else n * fac (n - 1)
{-
fix fact 0
-->
fact (fix fact) 0
=
(\fac -> \n ->
 if n == 0 then 1 else n * fac (n - 1)) (fix fact) 0
--> -->
if 0 == 0 then 1 else n * (fix fact) (n - 1)
-->
1

fix fact 2
-->
fact (fix fact) 2
=
(\fac -> \n ->
 if n == 0 then 1 else n * fac (n - 1)) (fix fact) 2
--> -->
if 2 == 0 then 1 else 2 * (fix fact) (2 - 1)
-->
2 * (fix fact) 1

-}

{-
try to type check M N

recursively find that M : A -> B
recursively find that N : A'

need to check whether A = A'?

concrete example (with type variables a and b):
M : (a -> b) -> (a -> b)
N : (Int -> Int)

is (a -> b) = (Int -> Int) ?
No, but if I substitute a |-> Int and b |-> Int, then it's fine!

--> Need substitute a and b in *whole* type for M:

M : (Int -> Int) -> (Int -> Int)
N : (Int -> Int)
----------------------------------- by rule App
M N : (Int -> Int)

--- another:

M : (a -> b) -> (a -> b)
N : (c -> Bool)

is (a -> b) =.= (c -> Bool) ?

no, but if I substitute a |-> c and b |-> Bool,
then I can "unify" the two types:

M : (c -> Bool) -> (c -> Bool)
N : (c -> Bool)

----

M N : (c -> Bool)

In each case, the substitution is called a unifier of the two types.

-}
