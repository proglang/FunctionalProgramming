import Std

namespace Course.Week08

inductive Expr where
  | lit (n : Int)
  | add (left right : Expr)
  | divide (left right : Expr)
  deriving Repr

def eval : Expr → Except String Int
  | .lit n => .ok n
  | .add left right => do
      let x ← eval left
      let y ← eval right
      pure (x + y)
  | .divide left right => do
      let x ← eval left
      let y ← eval right
      if y == 0 then throw "division by zero" else pure (x / y)

#eval eval (.add (.lit 4) (.lit 5))
#eval eval (.divide (.lit 8) (.lit 0))

end Course.Week08
