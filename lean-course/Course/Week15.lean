import Std

namespace Course.Week15

abbrev LoseState := StateT Nat (Except String)
abbrev KeepState := ExceptT String (StateM Nat)

def failLose : LoseState Unit := do
  modify (· + 1)
  throw "stopped"

def failKeep : KeepState Unit := do
  modify (· + 1)
  throw "stopped"

#eval failLose.run 0
#eval failKeep.run.run 0

end Course.Week15
