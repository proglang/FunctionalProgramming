import Std
namespace Course.Template02
inductive Suit where | clubs | diamonds | hearts | spades
structure Card where
  suit : Suit
  rank : Nat
-- LIVE: Complete both pattern-matching functions.
def isRed : Suit → Bool := sorry
def countRed : List Card → Nat := sorry
end Course.Template02
