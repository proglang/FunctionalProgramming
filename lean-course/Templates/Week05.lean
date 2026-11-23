import Std
namespace Course.Template05

#check IO.getStdout

def main : IO Unit := sorry

open IO.FS.Stream

#check putStrLn
#check getLine
open IO.FS
#check readFile
#check writeFile

def ior : α → IO α := fun x => pure x

def putStrLn' (s : String) : IO Unit :=
  IO.getStdout >>= fun stdout => stdout.putStrLn s
def putStrLn'' (s : String) : IO Unit :=
  bind IO.getStdout fun stdout => stdout.putStrLn s

#check (· >>= ·)
#check bind

def readFiles (f1 f2 : System.FilePath) : IO String :=
  readFile f1 >>= fun s1 => readFile f2 >>= fun s2 =>
  pure (s1 ++ s2)



def add42 (x : Nat) : Nat :=
  let y := 42
  y+x

end Course.Template05
