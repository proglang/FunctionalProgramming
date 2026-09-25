import Std

namespace Course.Week12

syntax "twice(" term ")" : term

macro_rules
  | `(twice($x)) => `($x + $x)

syntax "close_simple" : tactic

macro_rules
  | `(tactic| close_simple) => `(tactic| first | rfl | simp)

#eval twice(21)

example : twice(2) = 4 := by
  close_simple

example (n : Nat) : n + 0 = n := by
  close_simple

end Course.Week12
