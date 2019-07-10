
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
