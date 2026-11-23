import Std
namespace Course.Gauss

#eval List.sum (List.range 10)

def sum_of_n (n : Nat) : Nat :=
  match n with
  | 0 => 0
  | Nat.succ m => sum_of_n m + n

theorem gauss_sum (n : Nat) :
   (n * (n + 1)) / 2 = sum_of_n n := by
  induction n with
  | zero =>
    rfl
  | succ n ih =>
    unfold sum_of_n
    rw [← ih]
    simp


end Course.Gauss
