import Std
namespace Course.Template09
def append : List α → List α → List α
  | [], ys => ys
  | x :: xs, ys => x :: append xs ys
-- LIVE: Read the induction hypothesis, then finish the cons case.
theorem append_nil (xs : List α) : append xs [] = xs := by
  sorry
end Course.Template09
