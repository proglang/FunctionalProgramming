import Std

namespace Course.Week16

def compose (f : α → β) (g : β → γ) : α → γ := fun x => g (f x)

theorem compose_left_id (f : α → β) : compose id f = f := by
  funext x
  rfl

theorem compose_assoc (f : α → β) (g : β → γ) (h : γ → δ) :
    compose (compose f g) h = compose f (compose g h) := by
  funext x
  rfl

theorem option_map_id (x : Option α) : id <$> x = x := by
  cases x <;> rfl

theorem option_map_comp (f : α → β) (g : β → γ) (x : Option α) :
    (compose f g) <$> x = g <$> (f <$> x) := by
  cases x <;> rfl

#print axioms compose_assoc
#print axioms option_map_comp

end Course.Week16
