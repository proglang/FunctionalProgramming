> {-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, IncoherentInstances, DeriveFunctor #-}
> module M17alacarte where

* Data types a la carte

Based on:
Data Types a la Carte
Wouter Swierstra
Journal of Functional Programming 18 (4): 423-436, 2008.

Defining a recursive data type is straightforward in Haskell.
Defining functions on the data type (by recursion) is also straightforward.
But this style of definition is not modular:
what happens to these functions if we want to extend the data type by new constructors?

This question has been posed by Phil Wadler as the "Expression Problem":

"The goal is to define a data type by cases, where one can add new
cases to the data type and new functions over the data type, without
recompiling existing code, and while retaining static type safety."

Data types a la carte offers one solution.
First, we dissect the definition of a data type into its primitive parts.
The primitive parts turn out to be functors that specify primitive type constructions
including a functor that takes a fixed point at the level of types.
Second, we exploit the functor properties to define functions as certain algebras.
These functions extend automatically to all types as long as they are defined on the primitive type constructions. 

** Example definition

> data Exp0
>   = Int0 Int
>   | Add0 Exp0 Exp0

This definition can be read as defining a type Exp0 such that

Exp0 = Int0 Int | Add0 Exp0 Exp0

or (if we discard the constructor names)

Exp0 = Int + Exp0 x Exp0

A functor that describes the structure of Exp0 is expressed as a data type with an
additional type parameter.
It abstracts over the recursive uses of Exp0.

> data ExpF x
>   = IntF Int
>   | AddF x x
>   -- Regard this as a signature with a constant and a binary operator.
>   -- to add a unary operator:
>   | NegF x

Here is the kind of this datatype.

ExpF :: * -> *

We make it into a functor by defining its action on functions
via the @fmap@ function. 

> instance Functor ExpF where
>   fmap g (IntF i)     = IntF i
>   fmap g (AddF x1 x2) = AddF (g x1) (g x2)
>   fmap g (NegF x)     = NegF (g x)

To obtain a data type isomorphic to the original one, we introduce a
"fixed point at the type level" in analogy to the fixed point at the
value level that we know from lambda calculus. Remember that the
Y combinator is not typeable in Haskell, so we cannot put it to work here.
However, the fixed point is defineable with a recursive definition:

> fix :: (a -> a) -> a
> fix f = f (fix f)


Interestingly, the same construction works at the type level, too:

> data Mu f = In (f (Mu f))

Here, f :: * -> * does *not* stand for a type, but for a type function!
BTW: Mu :: (* -> *) -> *



Now we can construct a type which behaves like (is  "isomorphic" to) Exp0:

> type Exp1 = Mu ExpF

Exp1
= Mu ExpF
= In (ExpF (Mu ExpF))
= In (ExpF Exp1)
= In (IntF Int | AddF Exp1 Exp1 | NegF Exp1)

Same as Exp0, except for the additional In constructor!

Exp0 = Int0 Int | Add0 Exp0 Exp0

> int01 :: Int -> Exp1
> int01 i = In (IntF i)
>
> add01 :: Exp1 -> Exp1 -> Exp1
> add01 x y = In (AddF x y)
>
> neg01 :: Exp1 -> Exp1
> neg01 x = In (NegF x)







** Defining operations

A description of an operation on one level of a functor f :: * -> *,
is called an "f-algebra" (for Functor f):

> type Algebra f a = f a -> a

with kinding

f :: * -> *
a :: * 

Now we can provide an interpretation of the datatype ExpF in terms
of an f-algebra as in this example:

> evalExpAlg :: Algebra ExpF Int
> evalExpAlg (IntF i) = i
> evalExpAlg (AddF x1 x2) = x1 + x2
> evalExpAlg (NegF x) = -x
>
> printExpAlg :: Algebra ExpF String
> printExpAlg (IntF i) = show i
> printExpAlg (AddF x1 x2) = "(" ++ x1 ++ " + " ++ x2 ++ ")"
> printExpAlg (NegF x) = "(- " ++ x ++")"

This definition is *not* recursive!

Side remark:
f-algebras form a category and many recursive data types may be
described by an initial f-algebra for suitable f.

The point of this development is that we can define
a generic recursion operator that lifts any f-algebra to the fixpoint of f, Mu f.

> foldMu :: Functor f => Algebra f a -> (Mu f -> a)
> foldMu alg (In x) = alg (fmap (foldMu alg) x)

Type checking...
x :: f (Mu f)
alg :: f a -> a
fmap (foldMu alg) :: f (Mu f) -> f a

Application of the construction to Exp1.

> eval1 :: Exp1 -> Int
> eval1 = foldMu evalExpAlg

If we use a different algebra, we obtain another interpretation of the Exp1 data type.

> sizeExpAlg :: Algebra ExpF Int
> sizeExpAlg (IntF i) = 1
> sizeExpAlg (AddF x y) = 1 + x + y
> sizeExpAlg (NegF x) = 1 + x
>
> size1 = foldMu sizeExpAlg

> strExpAlg :: Algebra ExpF String
> strExpAlg (IntF i) = show i
> strExpAlg (AddF x y) = "(" ++ x ++ " + " ++ y ++ ")"
> strExpAlg (NegF x) = "(- " ++ x ++ ")"
>
> str1 = foldMu strExpAlg


> one' = In $ IntF 1
> two' = In $ AddF one' one'
> three' = In $ AddF one' two'

> prAlg :: Algebra ExpF String
> prAlg (IntF i) = show i
> prAlg (AddF x1 x2) = '(' : x1 ++ " + " ++ x2 ++ ")"
> prAlg (NegF x) = '(' : '-' : x ++ ")"

Convenience.
Using the injection In is tedious: use smart constructors instead

> int0 :: Int -> Exp1
> int0 i = In (IntF i)

> add0 :: Exp1 -> Exp1 -> Exp1
> add0 e1 e2 = In (AddF e1 e2)

* Another example: lists of integers

This example produces a datatype that is isomorphic to [Int].

> data ListF x = NilF | ConsF Int x
> instance Functor ListF where
>   fmap g NilF = NilF
>   fmap g (ConsF i x) = ConsF i (g x)
> type List1 = Mu ListF

> nil1 = In NilF
> cons1 i x = In (ConsF i x)

Examples of ListF algebras

> lengthAlg :: Algebra ListF Int
> lengthAlg NilF = 0
> lengthAlg (ConsF _ x) = 1 + x

> sumAlg :: Algebra ListF Int
> sumAlg NilF = 0
> sumAlg (ConsF i x) = i + x
>
> ilength :: Mu ListF -> Int
> ilength = foldMu lengthAlg
>
> isum :: List1 -> Int
> isum = foldMu sumAlg
>
> prListAlg :: Algebra ListF String
> prListAlg NilF = "[]"
> prListAlg (ConsF i x) = "(" ++ show i ++ " : " ++ x ++ ")"


Type-parametric lists

> data PListF a x = PNilF | PConsF a x
> instance Functor (PListF a) where
>   fmap g PNilF = PNilF
>   fmap g (PConsF a x) = PConsF a (g x)

> psumAlg :: (Num a) => Algebra (PListF a) a
> psumAlg PNilF = 0
> psumAlg (PConsF a x) = a + x

* Yet another example: binary trees

> data BTreeF a x = BEmptyF | BNodeF x a x
>   deriving (Functor)
>
> bsumAlg :: (Num a) => Algebra (BTreeF a) a
> bsumAlg BEmptyF = 0
> bsumAlg (BNodeF l a r) = l + a + r


As another example, consider a data type representing abstract syntax and
some functions on that syntax, e.g., a pretty printer, an interpreter,
...

* Building blocks for functors

We can define a range of data types just by providing different
functors.
Start with the constant functor.

> data Val e = Val Int
> type ConstExpr = Mu Val
>
> instance Functor Val where
>   fmap g (Val i) = Val i

Mu Val is isomorphic to Int:

Mu Val ~= Val (Mu Val) ~= Int

> data Add e = Add e e
> type AddExpr = Mu Add

> instance Functor Add where
>   fmap f (Add x y) = Add (f x) (f y)

Mu Add ~= Add (Mu Add) (Mu Add) ~= ...

> ccc :: ConstExpr
> ccc = In $ Val 1
>
> aaa :: AddExpr
> aaa = In $ Add aaa aaa

To combine the two functors, we need to take their
"coproduct" on the level of functors:
(better known as sum type to functional programmers)

> infixr :+:
> data (f :+: g) e = Inl (f e) | Inr (g e)
> type AddConstExpr = Mu (Val :+: Add)

f :: * -> *
g :: * -> *
e :: *
(f :+: g) :: * -> *


What would an example expression look like?

> cst' :: Int -> AddConstExpr
> cst' n = In $ Inl $ Val n
>
> add' :: AddConstExpr -> AddConstExpr -> AddConstExpr
> add' e1 e2 = In $ Inr $ Add e1 e2



** Evaluation

First, we need to make explicit that Val and Add are functors and that
the coproduct of two functors is again a functor.

 instance Functor Val where
   fmap f (Val i) = (Val i)

> instance (Functor f, Functor g) => (Functor (f :+: g)) where
>   fmap h (Inl e) = Inl (fmap h e)
>   fmap h (Inr e) = Inr (fmap h e)


Here are some specific examples.

> class Functor f => Eval f a where
>   evalAlgebra :: f a -> a

> instance Eval Val Int where
>   evalAlgebra (Val i) = i
> instance Eval Add Int where
>   evalAlgebra (Add x y) = x + y







What about the combined case?

> instance (Eval f a, Eval g a) => Eval ((:+:) f g) a where
>   evalAlgebra (Inl x) = evalAlgebra x
>   evalAlgebra (Inr y) = evalAlgebra y

Now we can put together an eval function.

> eval :: Eval f a => Mu f -> a
> eval e = foldMu evalAlgebra e

Does it work?

** Injection

Writing 
In (Inl (Val 17))
In (Inr (Add (In (Inl (Val 17))) (In (Inl (Val 4)))))
is quite unreadable.

> data Neg e = Neg e
>   deriving Functor
>
> instance Eval Neg Int where
>   -- evalAlgebra :: Neg Int -> Int
>   evalAlgebra (Neg x) = -x

> type AddConstNegExpr = Mu (Val :+: (Add :+: Neg))
> ttt :: AddConstNegExpr
> ttt = In (Inr (Inr (Neg (In (Inl (Val 5))))))











How about using smart constructors like the following?

> val1 :: Int -> Mu Val
> val1 x = In (Val x)

> add1 :: Mu Add -> Mu Add -> Mu Add
> add1 x y = In (Add x y)

Unfortunately, they do not combine in a data type like Mu (Val :+: Add)!

We need to provide these constructors with a more general type. In
particular, the constructor should work regardless how deeply nested
the functor occurs in a nest of :+: applications. Hence, we define a
subtype relation that locates a functor in such a nest and that
generates the correct sequence of Inl and Inr constructors on the way.

To express this relation requires an extended notion of a type class
with multiple parameters. Such a class describes an n-ary relation on
types, where a standard type class only describes a unary relation (a
predicate). 

> class sub :<: super where
>   inj :: sub a -> super a

At the functor itself, no injection is needed.
(*) this instance declaration is illegal by default because its form
may give rise to ambiguities. 

> instance f :<: f where
>   inj = id

Otherwise, we may find the primitive functor either on the left or on
the right.

> instance f :<: (f :+: g) where {-I2-}
>   inj = Inl
> instance (f :<: g) => f :<: (h :+: g) where {-I3-}
>   inj = Inr . inj

This definition is not really declarative. It requires that primitive
functors occur directly in the left alternative of a coproduct or
nested in the right alternative. Supporting nesting in both
alternatives would require Haskell to search exhaustively for suitable
instances. However, the Haskell type checker only checks the first
applicable alternative. 

With this class, suitable smart constructors are definable.

> inject :: (g :<: f) => g (Mu f) -> Mu f
> inject = In . inj

> val :: (Val :<: f) => Int -> Mu f
> val x = inject (Val x)

> add :: (Add :<: f) => Mu f -> Mu f -> Mu f
> add x y = inject (Add x y)

> neg :: (Neg :<: f) => Mu f -> Mu f
> neg x = inject (Neg x)

These constructors improve readability a lot.

Hack: make eval instance for ListF

> instance Eval ListF Int where
>   evalAlgebra (NilF) = 0
>   evalAlgebra (ConsF i x) = i+x
>
> xnil :: (ListF :<: f) => Mu f
> xnil = inject NilF
>
> xcons :: (ListF :<: f) => Int -> Mu f -> Mu f
> xcons i x = inject (ConsF i x)

** Extension

Now for the litmus test. Let's add new features to the AST.

> data Mul e = Mul e e
>   deriving Functor

> instance Eval Mul Int where
>   evalAlgebra (Mul x y) = x * y

> mul :: (Mul :<: f) => Mu f -> Mu f -> Mu f
> mul x y = inject (Mul x y)

> type AddConstNegMulExpr = Mu (Val :+: (Add :+: (Neg :+: Mul)))

add (val 1) (val 2) :: (Val :<: f, Add :<: f) => Mu f

f = Val :+: (Add :+: Neg)

type checker tries to prove:
(Val :<: (Val :+: (Add :+: Neg)))
by {-I2-}
inj = Inl

(Add :<: (Val :+: (Add :+: Neg)))
by {-I3-}
(Add :<: (Add :+: Neg))
by {-I2-}
inj = Inr . Inl


** Extension II

Adding new functions may be done either by defining more algebras and
using foldMu or by using *open-ended recursion*  directly.

Example: A pretty printer transforms expressions into strings.

> class Render f where
>   render :: Render g => f (Mu g) -> String

Why can't we simply use Mu f for the recursive use?

Given that render is defined, the pretty printer boils down to:

> pretty :: Render f => Mu f -> String
> pretty (In x) = render x

x :: f (Mu f)

In case of Mu (Val :+: Add)
x :: (Val :+: Add) (Mu (Val :+: Add))
x.left  :: Val (Mu (Val :+: Add))
x.right :: Add (Mu (Val :+: Add))

It remains to define instances of Render for each functor.

> instance Render Val where
>   render (Val i) = show i

> instance Render Add where
>   render (Add x y) = pretty x ++ " + " ++ pretty y

> instance Render Mul where
>   render (Mul x y) = pretty x ++ " * " ++ pretty y

> instance (Render f, Render g) => Render (f :+: g) where
>   render (Inl x) = render x
>   render (Inr y) = render y

> instance Render Neg where
>   render (Neg x) = '-':pretty x



** An application to free monads

For any functor f, this data structure is a monad, the *free monad* for f:

> data Term f a
>   = Pure a
>   | Impure (f (Term f a))

It distinguishes a pure value from an impure effect described by f.




> instance Functor f => Functor (Term f) where
>   fmap h (Pure a) = Pure (h a)
>   fmap h (Impure ftf) = Impure (fmap (fmap h) ftf)

h :: a -> b
result :: Term f a -> Term f b
t :: f (Term f a)

ftf :: f (Term f a)

> thenTerm :: Functor f => Term f a -> (a -> Term f b) -> Term f b
> thenTerm (Pure a) g = g a
> thenTerm (Impure ftfa) g = Impure (fmap (`thenTerm` g) ftfa)

> instance Functor f => Applicative (Term f) where
>   pure a = Pure a
>   ag <*> ax = ag `thenTerm` (\g -> ax `thenTerm` (\x -> Pure $ g x))

ag :: f (a -> b)
ax :: f a
ag <*> ax :: f b

> instance Functor f => Monad (Term f) where
>   (>>=) = thenTerm

Most monads are not free, for instance, the list and state monads are
not. But some well-known monads may be constructed as free monads:

> data Zero a
> data One a = One
> data Const e a = Const e

Can we construct elements of `Term Zero a`?
- Pure 1 :: Term Zero Int
Isomorphic to the Identity functor.

Elements of `Term One a`:
- Pure x :: Term One x
- Impure One
Isomorphic to Maybe a!

Elements of `Term (Const e) a`:
::: Pure :: a -> Term (Const e) a
- Pure a :: Term (Const e) a
::: Impure :: Const e (Term (Const e) a) -> Term (Const e) a
- Impure (Const e) 
Isomorphic to Either a e!



data Term f a
   = Pure a
   | Impure (f (Term f a))

TermZero a = Pure a

TermOne a = Pure a | Impure One

Term (Const e) a = Pure a | Impure (Const e (Term (Const e) a))




data TermConst a e = Pure a | ImpureConst e
... Either, error monad 

> instance Functor Zero where
>   fmap f _ = undefined

data TermZero a = Pure a 
... identity monad

> instance Functor One where
>   fmap f One = One

data TermOne a = Pure a | ImpureOne
... the Maybe type

The trick of this construction is that (the functor) Term maps
functors to monads. Furthermore, it is left-adjoint to the forgetful
functor from monads to functors and thus preserves coproducts. Hence,
coproducts of functors (that we just constructed in the previous
sections) map to coproducts of monads!
Thus, the syntax defined in this way is directly monadic.


As an example, consider a calculator with three actions on a memory cell.
RECALL: obtain the current value from memory.
INCREMENT: increment the value in memory by a given amount.
CLEAR: reset memory to zero.

> data Incr t   = Incr Int t
> data Recall t = Recall (Int -> t)
> data Clear t  = Clear t
> data Put t    = Put Int t

> instance Functor Incr where
>   fmap f (Incr x t) = Incr x (f t)
> instance Functor Recall where
>   fmap f (Recall h) = Recall (f . h)
> instance Functor Clear where
>   fmap f (Clear t) = Clear (f t)
> instance Functor Put where
>   fmap f (Put x t) = Put x (f t)

To write terms using this operators, we define smart constructors.

> injectT :: (g :<: f) => g (Term f a) -> Term f a
> injectT = Impure . inj

> incr :: (Incr :<: f) => Int -> Term f ()
> incr x = injectT (Incr x (Pure ()))

> recall :: (Recall :<: f) => Term f Int
> recall = injectT (Recall Pure)

> clear :: (Clear :<: f) => Term f ()
> clear = injectT (Clear (Pure ()))

> put :: (Put :<: f) => Int -> Term f ()
> put x = injectT (Put x (Pure ()))

Now, we can use these operators in a monad.

> tick1 :: Term (Recall :+: Incr) Int
> tick1 = do y <- recall
>            incr 1
>            return y

> tick2 :: (Functor f, Recall :<: f, Incr :<: f) => Term f Int
> tick2 = do y <- recall
>            incr 1
>            return y

> tick3 :: Term (Incr :+: Recall) Int
> tick3 = do y <- recall
>            incr 1
>            return y

We write functions over terms with a suitable fold operator.

> foldTerm :: Functor f => (a -> b) -> (f b -> b) -> Term f a -> b
> foldTerm f alg (Pure a) = f a
> foldTerm f alg (Impure t) = alg (fmap (foldTerm f alg) t)

second clause:
t :: f (Term f a)
foldTerm f alg :: Term f a -> b
fmap (foldTerm f alg) :: f (Term f a) -> f b
fmap (foldTerm f alg) t :: f b

To run terms with Incr and Recall, we need to define a data type for
the memory and a state monad to interpret terms in.

> newtype Mem = Mem Int
>   deriving Show



So that 
run :: (...) => Term f a -> Mem -> (a, Mem)

> class Functor f => Run f where
>   runAlgebra :: f (Mem -> (a, Mem)) -> (Mem -> (a, Mem))


> instance Run Incr where
>   runAlgebra (Incr k cont) = \ (Mem i) -> cont (Mem (i+k))

> instance Run Recall where
>   runAlgebra (Recall callback) = \ (Mem i) -> (callback i) (Mem i)

> instance Run Clear where
>   runAlgebra (Clear cont) = \ (Mem i) -> cont (Mem 0)

> instance Run Put where
>   runAlgebra (Put k cont) = \ (Mem i) -> cont (Mem k)

> instance (Run f, Run g) => Run (f :+: g) where
>   runAlgebra (Inl r) = runAlgebra r
>   runAlgebra (Inr r) = runAlgebra r

> run :: Run f => Term f a -> Mem -> (a, Mem)
> run = foldTerm (,) runAlgebra

(,) :: a -> b -> (a, b)
return :: a -> Mem -> (a, Mem)


What did we gain?

> tick4 :: Term (Incr :+: Recall :+: Clear) Int
> tick4 = tick2

> tick5 :: Term (Incr :+: Recall :+: Clear :+: Put) Int
> tick5 = do y <- recall
>            put 4711
>            return y
