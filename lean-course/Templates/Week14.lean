import Std
namespace Course.Template14
inductive Ty where
  | nat | variable (id : Nat) | arrow (input output : Ty)
  deriving Repr, DecidableEq
-- BONUS LIVE: Reject a substitution if the variable occurs inside its replacement.
def occurs (id : Nat) : Ty → Bool := sorry
def decompose (left right : Ty) : Except String (List (Ty × Ty)) := sorry
end Course.Template14
