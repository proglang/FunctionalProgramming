import Std
namespace Course.Template11
inductive Ty where | nat | bool
abbrev denote : Ty → Type
  | .nat => Nat
  | .bool => Bool
inductive Expr : Ty → Type where
  | number (n : Nat) : Expr .nat
  | boolean (b : Bool) : Expr .bool
  | add (x y : Expr .nat) : Expr .nat
  | choose (condition : Expr .bool) (yes no : Expr t) : Expr t
-- LIVE: The result type is determined by the index t.
def eval : (t : Ty) → Expr t → denote t := sorry
end Course.Template11
