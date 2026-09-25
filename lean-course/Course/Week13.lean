import Std

namespace Course.Week13

-- Teacher-owned specification: preserve order and membership in each part.
def partitionBy (p : α → Bool) : List α → List α × List α
  | [] => ([], [])
  | x :: xs =>
      let (yes, no) := partitionBy p xs
      if p x then (x :: yes, no) else (yes, x :: no)

theorem partitionBy_yes (p : α → Bool) (xs : List α) :
    (partitionBy p xs).1 = xs.filter p := by
  induction xs with
  | nil => rfl
  | cons x xs ih =>
      simp [partitionBy, ih]
      split <;> simp_all

theorem partitionBy_no (p : α → Bool) (xs : List α) :
    (partitionBy p xs).2 = xs.filter (fun x => !p x) := by
  induction xs with
  | nil => rfl
  | cons x xs ih =>
      simp [partitionBy, ih]
      split <;> simp_all

#eval partitionBy (· % 2 == 0) [1, 2, 3, 4, 5]
#print axioms partitionBy_yes
#print axioms partitionBy_no

end Course.Week13
