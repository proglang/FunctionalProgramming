import Std

namespace Course.Week05

inductive TrafficLight where
  | red | amber | green
  deriving Repr, DecidableEq

class Label (α : Type) where
  label : α → String

instance : Label TrafficLight where
  label
    | .red => "stop"
    | .amber => "wait"
    | .green => "go"

instance : Label Nat where
  label n := toString n

def describe [Label α] (x : α) : String := Label.label x

#eval describe TrafficLight.green
#eval describe (7 : Nat)
#check describe

end Course.Week05
