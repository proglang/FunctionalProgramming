import Std

namespace Course.Week03

def mapList (f : α → β) : List α → List β
  | [] => []
  | x :: xs => f x :: mapList f xs

def filterList (p : α → Bool) : List α → List α
  | [] => []
  | x :: xs => if p x then x :: filterList p xs else filterList p xs

def foldRight (f : α → β → β) (base : β) : List α → β
  | [] => base
  | x :: xs => f x (foldRight f base xs)

def sum (xs : List Nat) : Nat := foldRight (· + ·) 0 xs

#eval mapList (· * 2) [1, 2, 3]
#eval filterList (· > 2) [1, 2, 3, 4]
#eval sum [1, 2, 3]

theorem mapList_nil (f : α → β) : mapList f [] = [] := by
  rfl

end Course.Week03
