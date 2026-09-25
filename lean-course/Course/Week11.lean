import Std

namespace Course.Week11

inductive Ty where
  | nat | bool

abbrev denote : Ty → Type
  | .nat => Nat
  | .bool => Bool

inductive Expr : Ty → Type where
  | number (n : Nat) : Expr .nat
  | boolean (b : Bool) : Expr .bool
  | add (x y : Expr .nat) : Expr .nat
  | isZero (x : Expr .nat) : Expr .bool
  | choose (condition : Expr .bool) (yes no : Expr t) : Expr t

def eval : (t : Ty) → Expr t → denote t
  | .nat, .number n => n
  | .bool, .boolean b => b
  | .nat, .add x y => eval .nat x + eval .nat y
  | .bool, .isZero x => eval .nat x == 0
  | t, .choose condition yes no =>
      if eval .bool condition then eval t yes else eval t no

def sample : Expr .nat :=
  .choose (.isZero (.number 0)) (.add (.number 2) (.number 3)) (.number 99)

#eval eval .nat sample

end Course.Week11
