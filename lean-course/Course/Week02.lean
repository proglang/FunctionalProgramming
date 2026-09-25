import Std

namespace Course.Week02

inductive Suit where
  | clubs | diamonds | hearts | spades
  deriving Repr, DecidableEq

inductive Rank where
  | number (n : Nat) | jack | queen | king | ace
  deriving Repr, DecidableEq

structure Card where
  suit : Suit
  rank : Rank
  deriving Repr

def isRed : Suit → Bool
  | .diamonds | .hearts => true
  | .clubs | .spades => false

def countRed : List Card → Nat
  | [] => 0
  | card :: rest => (if isRed card.suit then 1 else 0) + countRed rest

#eval countRed [⟨.hearts, .ace⟩, ⟨.spades, .king⟩, ⟨.diamonds, .number 3⟩]

theorem countRed_nil : countRed [] = 0 := by
  rfl

end Course.Week02
