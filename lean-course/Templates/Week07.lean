import Std
namespace Course.Template07
abbrev Parser (α : Type) := List Char → Option (α × List Char)
def character (wanted : Char) : Parser Char := sorry
-- LIVE: Preserve the unconsumed input.
def mapParser (f : α → β) (p : Parser α) : Parser β := sorry
def sequence (pf : Parser (α → β)) (pa : Parser α) : Parser β := sorry
end Course.Template07
