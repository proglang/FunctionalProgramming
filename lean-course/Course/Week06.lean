import Std

namespace Course.Week06

def safeDivide (x y : Nat) : Except String Nat :=
  if y == 0 then .error "division by zero" else .ok (x / y)

def ratioOfSums (a b c d : Nat) : Except String Nat := do
  let numerator := a + b
  let denominator := c + d
  safeDivide numerator denominator

def greet (name : String) : IO Unit := do
  IO.println s!"Hello, {name}!"

#eval ratioOfSums 10 2 1 2
#eval ratioOfSums 10 2 0 0

end Course.Week06
