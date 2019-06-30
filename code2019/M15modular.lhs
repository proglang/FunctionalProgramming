> {-# LANGUAGE FlexibleContexts #-}

> module M15modular where
> import Control.Monad.Except
> import Control.Monad.Reader

Modular interpreters
====================

We learned about several uses of monads (applicatives, functors)
* to express various kinds of computational effect (state, reader, writer, ...)
* to write extensible interpreters
* to express composition of effects with monad transformers

Put this accumulated knowledge into practice by building an actual interpreter
step by step. In particular, we will see how to
* interpret an expression language (again)
* add exceptions
* add variable binding
* NEW: implement function values and function application
* NEW: implement lazy values

We achieve all that just by adding new code, never going back to change existing code.






Syntax

0.0 The base language

> data Exp
>   = Integer Integer
>   | Bool Bool
>   | String String
>   | Binop Binop Exp Exp






1.0 Exception handling

>   | Try Exp Exp







2.0 Variables

>   | Let Ident Exp Exp
>   | Var Ident






3.0 Functions

>   | Lam Ident Exp
>   | App Exp Exp




4.0 Laziness

>   | Delay Exp
>   | Force Exp






0.1 Values 

Now that we have several types of values, we have to encode them in a data type.

> data Value
>  = VInteger Integer
>  | VBool Bool
>  | VString String




3.1 Function values

Now we could try to represent functions by functions...

  | VFun (Value -> Value)

but that would not work, because we'd cut off the monad at the point where the function is called. The function should rather be monadic as in

  | VFun (Value -> m Value)

but then our Value type would have to be parameterized over the monad m. 

Fortunately, there is a simpler first-order representation of functions as a data structure: The closure

>  | VClosure Env Ident Exp





4.1 Lazy values

Lazy values are also represented by closures, but they don't have an argument.
They are evaluated later, but still with their current environment.

>  | VThunk Env Exp





0.2 Interpretation of the binary operators.

> data Binop = Add | Sub | Mul | Div | Eql | Cnc

> -- binop :: (Monad m) => Binop -> Value -> Value -> m Value
> binop Add (VInteger x1) (VInteger x2) =
>   return $ VInteger (x1 + x2)

> binop Eql (VInteger x1) (VInteger x2) =
>   return $ VBool (x1 == x2)

> binop Cnc (VString s1) (VString s2) =
>   return $ VString (s1 ++ s2)



1.1 An exceptional operation

Let's start with the exception
"
throwError :: e -> m a
    Is used within a monadic computation to begin exception processing.
"

> binop Div (VInteger x1) (VInteger x2) =
>   if x2 == 0 then throwError "division by zero" else return $ VInteger (x1 `div` x2)






2.1 Variable bindings

> type Ident = String

> type Env = Ident -> Maybe Value

> update :: Ident -> Value -> Env -> Env
> update x v e = \y -> if x == y then Just v else e x






0.3 A first round of interpretation

> -- eval :: (Monad m) => Exp -> m Value
> eval (Integer i) = return $ VInteger i
> eval (Bool b) = return $ VBool b
> eval (String s) = return $ VString s
> eval (Binop b e1 e2) = do
>   x1 <- eval e1
>   x2 <- eval e2
>   binop b x1 x2







1.2 Let's deal with the exception

"
catchError :: m a -> (e -> m a) -> m a

A handler function to handle previous errors and return to normal execution.
A common idiom is: 

do { action1; action2; action3 } `catchError` handler

where the action functions can call throwError.
Note that handler and the do-block must have the same return type.
"

In our language, Try should work similar to try in Python, say.

> eval (Try e1 e2) =
>   eval e1 `catchError` \ s -> eval e2

We don't have a means to pass the string to the evaluator, yet.



2.2 Variable bindings with a Reader monad

"
ask :: m r

    Retrieves the monad environment.
"

> eval (Var x) = do
>   env <- ask
>   maybe (throwError "unbound variable") return $ env x 

"
local :: (r -> r)    -- |The function to modify the environment.
         -> m a	     -- |Reader to run in the modified environment.
         -> m a	     -- |Executes a computation in a modified environment.
"

> eval (Let x e1 e2) = do
>   v1 <- eval e1
>   local (update x v1) (eval e2)





3.2 Function values

> eval (Lam x e) = do
>   env <- ask
>   return $ VClosure env x e

That is, the function value remembers (in its env component) the environment (i.e. the values of the variables) where it was created. This environment has to be reinstalled when the function is applied to an argument.

> eval (App e1 e2) = do
>   v1 <- eval e1
>   v2 <- eval e2
>   case v1 of
>     VClosure env x e ->
>       local (const (update x v2 env)) (eval e)
>     _ ->
>       throwError "function expected"





4.2 Creating and using lazy values

> eval (Delay e) = do
>   env <- ask
>   return $ VThunk env e
>
> eval (Force e) = do
>   v <- eval e
>   case v of
>     VThunk env e' ->
>       local (const env) (eval e')

Not quite "lazy" as in Haskell, as they are evaluated at every use (which is "call by name") whereas Haskell's implementation of laziness guarantees at most one evaluation. 
Providing such a guarantee requires a state monad that remembers whether a thunk has been evaluated already.


================================================================================

