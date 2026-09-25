import Std

namespace Course.Week10

-- De Bruijn indices avoid names and capture during substitution.
inductive Term where
  | var (index : Nat)
  | lam (body : Term)
  | app (function argument : Term)
  deriving Repr

def wellScoped (depth : Nat) : Term → Bool
  | .var index => index < depth
  | .lam body => wellScoped (depth + 1) body
  | .app function argument => wellScoped depth function && wellScoped depth argument

-- Closures carry the environment in which a lambda was created.
inductive Value where
  | closure (body : Term) (environment : List Value)

def evalCBV : Nat → List Value → Term → Option Value
  | 0, _, _ => none
  | _fuel + 1, environment, .var index => environment[index]?
  | _, environment, .lam body => some (.closure body environment)
  | fuel + 1, environment, .app function argument => do
      let functionValue ← evalCBV fuel environment function
      let argumentValue ← evalCBV fuel environment argument
      match functionValue with
      | .closure body saved => evalCBV fuel (argumentValue :: saved) body

def identity : Term := .lam (.var 0)
def selfApplication : Term := .app identity identity

theorem identity_closed : wellScoped 0 identity = true := by
  rfl

#eval wellScoped 0 identity
#eval wellScoped 0 (.var 0)
#eval wellScoped 0 selfApplication
#eval (evalCBV 8 [] selfApplication).isSome

end Course.Week10
