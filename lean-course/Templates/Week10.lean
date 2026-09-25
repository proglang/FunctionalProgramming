import Std
namespace Course.Template10
inductive Term where
  | var (index : Nat)
  | lam (body : Term)
  | app (function argument : Term)
-- LIVE: Entering a lambda adds one bound variable.
def wellScoped (depth : Nat) : Term → Bool := sorry
inductive Value where
  | closure (body : Term) (environment : List Value)
-- LIVE: Use the fuel to justify recursive evaluation of a closure body.
def evalCBV : Nat → List Value → Term → Option Value := sorry
end Course.Template10
