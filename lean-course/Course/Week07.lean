import Std

namespace Course.Week07

abbrev Parser (α : Type) := List Char → Option (α × List Char)

def character (wanted : Char) : Parser Char
  | [] => none
  | c :: rest => if c == wanted then some (c, rest) else none

def mapParser (f : α → β) (p : Parser α) : Parser β := fun input => do
  let (value, rest) ← p input
  pure (f value, rest)

def sequence (pf : Parser (α → β)) (pa : Parser α) : Parser β := fun input => do
  let (f, rest) ← pf input
  let (a, remaining) ← pa rest
  pure (f a, remaining)

def twoChars (a b : Char) : Parser (Char × Char) :=
  sequence (mapParser (fun x y => (x, y)) (character a)) (character b)

#eval twoChars 'a' 'b' ['a', 'b', 'c']
#eval twoChars 'a' 'b' ['a', 'c']

end Course.Week07
