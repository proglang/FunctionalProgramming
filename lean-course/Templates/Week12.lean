import Std
namespace Course.Template12
syntax "twice(" term ")" : term
-- LIVE: Use syntax quotation and antiquotation.
macro_rules
  | `(twice($x)) => sorry
syntax "close_simple" : tactic
macro_rules
  | `(tactic| close_simple) => sorry
end Course.Template12
