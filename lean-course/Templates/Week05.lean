import Std
namespace Course.Template05
inductive TrafficLight where | red | amber | green
class Label (α : Type) where
  label : α → String
-- LIVE: Supply an instance, then call describe at two types.
instance : Label TrafficLight where
  label := sorry
def describe [Label α] (x : α) : String := sorry
end Course.Template05
