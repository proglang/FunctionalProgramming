

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
