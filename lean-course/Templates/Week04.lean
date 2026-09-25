import Std
namespace Course.Template04
inductive Tree (α : Type) where
  | leaf
  | node (left : Tree α) (value : α) (right : Tree α)
-- LIVE: Each subtree contributes its own size.
def Tree.size : Tree α → Nat := sorry
-- LIVE: Use an accumulator for a tail-recursive reverse.
def reverseAcc (xs : List α) : List α := sorry
end Course.Template04
