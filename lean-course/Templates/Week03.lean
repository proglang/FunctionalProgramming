import Std
namespace Course.Template03
-- LIVE: Recursion first, then express sum using foldRight.
def mapList (f : α → β) : List α → List β := sorry
def foldRight (f : α → β → β) (base : β) : List α → β := sorry
def sum (xs : List Nat) : Nat := sorry
end Course.Template03
