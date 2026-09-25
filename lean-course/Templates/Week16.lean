import Std
namespace Course.Template16
def compose (f : α → β) (g : β → γ) : α → γ := fun x => g (f x)
-- BONUS LIVE: Function extensionality turns equality of functions into a pointwise goal.
theorem compose_assoc (f : α → β) (g : β → γ) (h : γ → δ) :
    compose (compose f g) h = compose f (compose g h) := by
  sorry
theorem option_map_comp (f : α → β) (g : β → γ) (x : Option α) :
    (compose f g) <$> x = g <$> (f <$> x) := by
  sorry
end Course.Template16
