> {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
> module M18generic where

* Generic programming in Haskell

Based on

Ralf Hinze
Generics for the Masses
ICFP 2004

Oliveira, Hinze, Löh
Extensible and Modular Generics for the Masses
TFP 2007

See also http://www.cs.uu.nl/wiki/GenericProgramming/EMGM

What is generic (polytypic) programming?

Generic programming attempts to define functions that work uniformly
on all datatypes *including those that are yet to be defined in the
future*.  Typical examples are pretty printers, parsers, size
functions, equality, compression, serialization, and so on.  In fact,
Haskell's deriving mechanism is a piece of generic programming, but it
is hardwired in the Haskell compiler. Here, we are interested in
user-definable generic programs.




** Example: Data Compression

A data compression function maps arbitrary values to sequences of bits.

> type Bin = [Bit]
> data Bit = O | I deriving Show
> bits :: Enum a => Int -> a -> Bin

The expression 'bits n x' represents an element x of an enumerated
type by a bit string of length n. The goal of this exercise is to lift
the bits function from just enums to a function showBin that works on
all types (excluding function types).

We will see that it is sufficient to define showBin for all primitive types
and for the elementary type constructors: the unit type, the sum type,
and the product type.

> data Unit = Unit
> data Plus a b = Inl a | Inr b
> data Pair a b = Pair { outl :: a, outr :: b }

The type signature of the generic function is (unusually) specified by
a data definition. 

> newtype ShowBin a = ShowBin { showBin' :: a -> Bin }

It cannot be a "real" polymorphic function because it does not work
(uniformly) for all types. A generic function has so-called
*intensional polymorphism* where the function analyzes the type of its
argument to determine the action.  Hence, the argument type of a
generic function is restricted (using a type class) to those types
that have a representation that the generic mechnism can deal
with. This representation for type a is provided as a value of type
ShowBin a.

So every generic function is defined as an instance of a type class
Generic where the members return a suitable representation.

> instance Generic ShowBin where
>   unit     = ShowBin (\_ -> [])
>   plus a b = ShowBin (\x -> case x of Inl l -> O : showBin' a l
>                                       Inr r -> I : showBin' b r)
>   pair a b = ShowBin (\x -> showBin' a (outl x) ++ showBin' b (outr x))
>   char     = ShowBin (\c -> bits 7 c)
>   int      = ShowBin (\i -> bits 16 i)
>   view iso = \r -> ShowBin (\x -> showBin' r (fromData iso x))

The last definition refers to an isomorphism iso that maps a type into
its elementary representation and back. It is required to represent
the actual type in terms of the primitive types and type constructors.

> data Iso a b = Iso { fromData :: b -> a, toData :: a -> b }


** Tasks
*** generic comparison :: a -> a -> Ordering
*** readBin :: Bin -> a, which is an inverse to showBin

> newtype ReadBin a = ReadBin { readBin' :: Bin -> (a, Bin) }

> instance Generic ReadBin where
>   unit         = ReadBin (\bs -> (Unit, bs))
>   plus rbl rbr = ReadBin (\(b:bs) -> case b of O -> let (vl, bs') = readBin' rbl bs in (Inl vl, bs')
>                                                I -> let (vr, bs') = readBin' rbr bs in (Inr vr, bs'))
>   pair rbl rbr = ReadBin (\bs -> let (vl, bsl) = readBin' rbl bs
>                                      (vr, bsr) = readBin' rbr bsl
>                                  in  (Pair vl vr, bsr))
>   char         = ReadBin (unbits 7)
>   int          = ReadBin (unbits 16)
>   view iso     = \r -> ReadBin (\bs -> let (gt, bs') = readBin' r bs in (toData iso gt, bs'))
>
> unbits :: Enum a => Int -> Bin -> (a, Bin)
> unbits n bs = let (v, bs') = unbitsInt n bs in (toEnum v, bs')
>
> unbitsInt :: Int -> Bin -> (Int, Bin)
> unbitsInt n (I:bs) | n > 0 = let (v, bs') = unbitsInt (n-1) bs in (2^(n-1) + v, bs')
> unbitsInt n (O:bs) | n > 0 = unbitsInt (n-1) bs
> unbitsInt n bs | n == 0 = (0, bs)


** Representing a type

As an example, consider a typical datatype of binary trees:

> data Tree a = Leaf a | Fork (Tree a) (Tree a)

Its toplevel structure can be expressed using the primitive type
constructors as follows.

> type TreeF a = Plus a (Pair (Tree a) (Tree a)) 

We need to construct an isomorphism from
Tree a <-> TreeF a

> isoTree = Iso fromTree toTree

> fromTree :: Tree a -> TreeF a
> fromTree (Leaf a) = Inl a
> fromTree (Fork l r) = Inr (Pair l r)

> toTree :: TreeF a -> Tree a
> toTree (Inl a) = Leaf a
> toTree (Inr (Pair l r)) = Fork l r

For use in a generic function we need to provide an appropriate
representation function. It takes a generic function on type a and
lifts it to a function of type Tree a. It does so by converting Tree a
to TreeF a on demand and composing the generic function out of the
elementary types and constructors as dictated by TreeF a.

> rTree :: Generic f => f a -> f (Tree a)
> rTree fa = view isoTree (plus fa (pair (rTree fa) (rTree fa)))





For another example, consider encoding the standard list datatype.

| data List a = Nil | Cons a (List a)

> type ListF a = Plus Unit (Pair a [a])

| iso :: [a] <-> ListF a

> isoList = Iso fromList toList

> fromList :: [a] -> ListF a 		-- Plus Unit (Pair a [a])
> fromList [] = Inl Unit
> fromList (x:xs) = Inr (Pair x xs)

> toList :: ListF a -> [a]
> toList (Inl Unit) = []
> toList (Inr (Pair x xs)) = x : xs

If List were just defined by a recursive equation

| type List' a = Plus Unit (Pair a (List' a))

then its representation could be constructed without invoking view
and the isomorphism.

| rList' a = plus unit (pair a (rList' a))

But neither the type nor the value definition is legal Haskell.

Here is the corresponding representation transformer instead.

> rList :: Generic f => f a -> f [a]
> rList fa = view isoList (plus unit (pair fa (rList fa)))






Yet another example: the maybe type.

> type MaybeF a = Plus Unit a

| iso :: Maybe a <-> MaybeF a

> isoMaybe = Iso fromMaybe toMaybe

> fromMaybe Nothing = Inl Unit
> fromMaybe (Just a) = Inr a

> toMaybe (Inl Unit) = Nothing
> toMaybe (Inr a) = Just a

> rMaybe :: Generic f => f a -> f (Maybe a)
> rMaybe fa = view isoMaybe (plus unit fa)


Booleans. Bool = Plus Unit Unit

> isoBool = Iso fromBool toBool

> fromBool :: Bool -> Plus Unit Unit
> fromBool False = Inl Unit
> fromBool True  = Inr Unit

> toBool :: Plus Unit Unit -> Bool
> toBool (Inl Unit) = False
> toBool (Inr Unit) = True

> rBool :: Generic f => f Bool
> rBool = view isoBool (plus unit unit)









** Task: provide REP instances for
*** data Shrub α β = Tip α | Node (Shrub α β) β (Shrub α β)

> data Rose α = Branch α [Rose α] 

> type RoseF a = Pair a [Rose a]
>
> isoRose = Iso fromRose toRose
>
> fromRose :: Rose a -> RoseF a
> fromRose (Branch a lra) = Pair a lra
>
> toRose :: RoseF a -> Rose a
> toRose (Pair a lra) = Branch a lra
>
> rRose :: Generic f => f a -> f (Rose a)
> rRose fa = view isoRose (pair fa (rList (rRose fa)))

> instance Rep a => Rep (Rose a) where
>   rep = rRose rep
>
> instance FRep Rose where
>   frep = rRose



** Implementation

> class Generic f where
>   unit   :: f Unit
>   plus   :: f a -> f b -> f (Plus a b)
>   pair   :: f a -> f b -> f (Pair a b)
>   constr :: Name -> Arity -> f a -> f a
>   constr _ _ = id
>   char   :: f Char
>   int    :: f Int
>   view   :: Iso a b -> f a -> f b





















** Automatically inferring a representation

> class Rep a where
>   rep :: (Generic f) => f a

> instance Rep Unit where
>   rep = unit

> instance (Rep a, Rep b) => Rep (Plus a b) where
>   rep = plus rep rep

> instance (Rep a, Rep b) => Rep (Pair a b) where
>   rep = pair rep rep

> instance Rep Char where
>   rep = char

> instance Rep Int where
>   rep = int

> instance Rep a => Rep [a] where
>   rep = rList rep

> instance Rep a => Rep (Tree a) where
>   rep = rTree rep

> instance Rep Bool where
>   rep = rBool

> showBin :: (Rep a) => a -> Bin
> showBin = showBin' rep


To support a new datatype, we need to implement a rep transformer for
the datatype and define a Rep instance for it.

** Problem: Extensibility

A problem with this approach is that it is hard to add special cases,
like a more efficient treatment of lists: they could be
length-encoded.

Solution 1: Specialized representation class for each generic
function. 

> class Generic a => GenericList a where

> class RepList a where
>   replist :: (GenericList f) => f a

Solution 2: Abstract over representation *and* the generic function at
the same time using a two-parameter class.

> class GRep f a where
>   grep :: f a

> instance (Generic f) => GRep f Unit where
>   grep = unit

> instance (Generic f, GRep f a, GRep f b) => GRep f (Plus a b) where
>   grep = plus grep grep

| instance GRep ShowBin a => GRep ShowBin [a] where
|   grep = ...


* Abstracting over a type constructor

A function that determines the size of a value is type-generic in a
different way. It needs to be applicable at the following types:

[a] -> Int 
Tree a -> Int

... or generally at type  f a -> Int
where f is a type constructor.

Here is an instance of Generic for counting elements in a container
data structure.

> newtype Count a = Count { count' :: a -> Int }

> instance Generic Count where
>   unit     = Count (const 0)
>   plus a b = Count (\x -> case x of Inl l -> count' a l
>                                     Inr r -> count' b r)
>   pair a b = Count (\x -> count' a (outl x) + count' b (outr x))
>   char     = Count (const 0)
>   int      = Count (const 0)
>   view iso a = Count (\b -> count' a (fromData iso b))

Just using this function generically results in the constant 0
function! To obtain interesting results, we need to provide a
representation for the type constructor.

> size :: FRep h => h a -> Int
> size = count' (frep (Count (const 1)))

> gsum :: FRep h => h Int -> Int
> gsum = count' (frep (Count id))

> class FRep h where
>   frep :: Generic f => f a -> (f (h a))

> instance FRep [] where
>   frep = rList

> instance FRep Tree where
>   frep = rTree

> instance FRep Maybe where
>   frep = rMaybe

** Utility 

> bits n a = let i = fromEnum a in intBits n i
> intBits n j = if n==0 then [] else intBits (n-1) (j `div` 2) ++ [if odd j then I else O]
> type Name = String
> type Arity = Int
