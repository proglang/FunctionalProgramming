import Std
namespace Course.Template02

abbrev Name := String
abbrev Title := String
abbrev Year := Nat
abbrev Age := Nat
abbrev Person := Name × Age

#eval ("Alice", 30)
#check ("Alice", 30)

abbrev Purchase := Name × Title × Year
def purchase := ("Alice", "The Great Gatsby", 2020)

def purchaseName (p : Purchase) : Name := p.1
def purchaseYear (p : Purchase) : Year := p.2.2
def purchaseTitleYear (p : Purchase) : Title × Year := p.2
def purchaseNameTitle (p : Purchase) : Name × Title := (p.1, p.2.1)

#check purchaseName purchase

inductive Suit where | clubs | diamonds | hearts | spades
  deriving BEq
inductive Rank where | numeric (n : Nat) | ace | jack | queen | king
  deriving BEq

def sameSuit (a b : Suit) : Bool := a == b
def rankBeats : Rank → Rank → Bool
  | .ace, _ => true
  | _, .ace => false
  | .king, _ => true
  | _, .king => false
  | .queen, _ => true
  | _, .queen => false
  | .jack, _ => true
  | _, .jack => false
  | .numeric n, .numeric m => n > m

#check Suit.clubs
--
structure Card where
  suit : Suit
  rank : Rank

def card1 : Card := { suit := .hearts, rank := .ace }
def card2 : Card := { suit := .spades, rank := .numeric 10 }

#check card1.suit
#check card2.rank
#eval Card.suit card1

def cardBeats (c1 c2 : Card) : Bool :=
  sameSuit c1.suit c2.suit && rankBeats c1.rank c2.rank

-- LIVE: Complete both pattern-matching functions.

def isRed : Suit → Bool
  | .clubs => false
  | .diamonds => true
  | .hearts => true
  | .spades => false

#eval isRed Suit.hearts

def isRed' (s : Suit) : Bool :=
  match s with
  | .clubs => false
  | .diamonds => true
  | .hearts => true
  | .spades => false

abbrev Hand := List Card
def chooseCard : Hand → Card
  | [] => { suit := .hearts, rank := .ace }
  | c :: _ => c
(*
inductive Option (α : Type) where
  | none : Option α
  | some : α → Option α
*)
def chooseCard1 : Hand → Option Card
  | [] => none
  | c :: _ => some c

inductive Hand'' where
  | last (c : Card)
  | next (c : Card) (h : Hand'')

inductive Hand' where
  | last : Card → Hand'
  | next : Card → Hand' → Hand'

def chooseCard2 : Hand' → Card
  | .last c => c
  | .next c _ => c

def chooseBeatingCardIfPossible (c : Card) : Hand' → Card
  | .last c' => c'
  | .next c' h' => if cardBeats c' c then c' else chooseBeatingCardIfPossible c h'

end Course.Template02
