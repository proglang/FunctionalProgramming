import Std
namespace Course.Template15
abbrev LoseState := StateT Nat (Except String)
abbrev KeepState := ExceptT String (StateM Nat)
-- BONUS LIVE: Increment, then fail, and compare the two results.
def failLose : LoseState Unit := sorry
def failKeep : KeepState Unit := sorry
end Course.Template15
