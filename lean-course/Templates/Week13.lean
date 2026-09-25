import Std
namespace Course.Template13
-- Keep these statements fixed while generating an implementation and proofs.
def partitionBy (p : α → Bool) : List α → List α × List α := sorry
theorem partitionBy_yes (p : α → Bool) (xs : List α) :
    (partitionBy p xs).1 = xs.filter p := by
  sorry
theorem partitionBy_no (p : α → Bool) (xs : List α) :
    (partitionBy p xs).2 = xs.filter (fun x => !p x) := by
  sorry
#print axioms partitionBy_yes
#print axioms partitionBy_no
end Course.Template13
