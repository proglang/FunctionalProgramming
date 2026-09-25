import Std

namespace Course.Week01

def double (n : Nat) : Nat := n + n

def larger (x y : Nat) : Nat := if x < y then y else x

#eval double 7
#eval larger 12 9
#check double
#check List.map

theorem double_zero : double 0 = 0 := by
  rfl

end Course.Week01
