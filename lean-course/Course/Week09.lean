import Std

namespace Course.Week09

def append : List α → List α → List α
  | [], ys => ys
  | x :: xs, ys => x :: append xs ys

def length : List α → Nat
  | [] => 0
  | _ :: xs => length xs + 1

theorem append_nil (xs : List α) : append xs [] = xs := by
  induction xs with
  | nil => rfl
  | cons x xs ih => simp [append, ih]

theorem length_append (xs ys : List α) :
    length (append xs ys) = length xs + length ys := by
  induction xs with
  | nil => simp [append, length]
  | cons x xs ih => simp [append, length, ih, Nat.add_comm, Nat.add_left_comm]

#eval append [1, 2] [3, 4]
#print axioms append_nil
#print axioms length_append

end Course.Week09
