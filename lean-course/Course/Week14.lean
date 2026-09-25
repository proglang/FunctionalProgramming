import Std

namespace Course.Week14

inductive Ty where
  | nat
  | variable (id : Nat)
  | arrow (input output : Ty)
  deriving Repr, DecidableEq

def occurs (id : Nat) : Ty → Bool
  | .nat => false
  | .variable other => id == other
  | .arrow input output => occurs id input || occurs id output

-- One constraint-decomposition step; the full unifier is an extension.
def decompose (left right : Ty) : Except String (List (Ty × Ty)) :=
  match left, right with
  | .nat, .nat => .ok []
  | .arrow a b, .arrow c d => .ok [(a, c), (b, d)]
  | .variable x, ty =>
      if left == ty then .ok []
      else if occurs x ty then .error "occurs check failed"
      else .ok [(left, ty)]
  | ty, .variable x =>
      if occurs x ty then .error "occurs check failed"
      else .ok [(.variable x, ty)]
  | _, _ => .error "different type constructors"

#eval decompose (.arrow (.variable 0) .nat) (.arrow .nat (.variable 1))
#eval decompose (.variable 0) (.arrow (.variable 0) .nat)

end Course.Week14
