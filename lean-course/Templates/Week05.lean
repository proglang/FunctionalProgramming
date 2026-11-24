import Std
namespace Course.Template05

#check IO.getStdout


open IO.FS.Stream

#check putStrLn
#check getLine
open IO.FS
#check readFile
#check writeFile

def main : IO Unit :=
  IO.getStdout >>= fun stdout =>
  putStrLn stdout "Hello world"

#eval main

def ior : α → IO α := fun x => pure x

def putStrLn' (s : String) : IO Unit :=
  IO.getStdout >>= fun stdout => stdout.putStrLn s
def putStrLn'' (s : String) : IO Unit :=
  bind IO.getStdout fun stdout => stdout.putStrLn s

def silent : IO Unit :=
  let sound := putStrLn' "Hello second world!"
  sound >>= fun _ => putStrLn' "Hello second world!"

#eval silent


#check (· >>= ·)
#check bind

def readFiles (f1 f2 : System.FilePath) : IO String :=
  readFile f1 >>= fun s1 => readFile f2 >>= fun s2 =>
  pure (s1 ++ s2)

def copyFile (src dst : System.FilePath) : IO Unit :=
  readFile src >>= fun s => writeFile dst s

def copyFile' (src dst : System.FilePath) : IO Unit := do
  let s ← readFile src
  writeFile dst s

def CopyNonEmptyFile (src dst : System.FilePath) : IO Unit := do
  let s ← readFile src
  if s != "" then
    writeFile dst s
  else
    pure ()

def doTwice (act : IO Unit) : IO Unit := do
  act
  act

def doNot(act : IO Unit) : IO Unit :=
  pure ()

def add42 (x : Nat) : Nat :=
  let y := 42
  y+x

#print Option

end Course.Template05
