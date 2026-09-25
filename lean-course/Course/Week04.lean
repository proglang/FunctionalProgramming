import Std

namespace Course.Week04

inductive Tree (α : Type) where
  | leaf
  | node (left : Tree α) (value : α) (right : Tree α)
  deriving Repr

def Tree.size : Tree α → Nat
  | .leaf => 0
  | .node left _ right => left.size + 1 + right.size

def Tree.map (f : α → β) : Tree α → Tree β
  | .leaf => .leaf
  | .node left x right => .node (left.map f) (f x) (right.map f)

def reverseAcc (xs : List α) : List α :=
  go xs []
where
  go : List α → List α → List α
    | [], acc => acc
    | x :: rest, acc => go rest (x :: acc)

-- Lean evaluates arguments before calling a function. Delay work explicitly when needed.
def delayed (f : Unit → Nat) : Nat := f ()

#eval (Tree.node (Tree.node .leaf 2 .leaf) 4 .leaf).size
#eval reverseAcc [1, 2, 3, 4]
#eval delayed (fun _ => 6 * 7)

end Course.Week04
