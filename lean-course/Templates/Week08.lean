import Std
namespace Course.Template08
inductive Expr where
  | lit (n : Int)
  | add (left right : Expr)
  | divide (left right : Expr)
-- LIVE: Handle errors without repeating pattern matches.
def eval : Expr → Except String Int := sorry
end Course.Template08
