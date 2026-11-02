import Std
namespace Course.Template03
#check []
#check [1]
#check [1, 2, 3]
#check ["a", "b", "c"]
#check [true, false, true]
#check [1, "two", false]
#eval [1, 2, 3] = [3, 2, 1]
#eval "Hearts".toList == ['H','e','a','r','t','s']
inductive Lst (α : Type) where
  | nil
  | cons (h : α) (t : Lst α)
#check List.length
#check List.append
#check List.flatten
#check List.take
#check List.drop
#check List.zip
#eval [1,2,3].take 2
-- #eval take 2 [1,2,3]
#eval [1,2,3,4].drop 2
#check List.map
#check List.filter
#check List.and
def words (s : String) : List String :=
  ((s.split Char.isWhitespace).toStringList).filter (· != "")

def lines (s : String) : List String :=
  s.lines.toStringList

def unwords (ss : List String) : String :=
  " ".intercalate ss

def unlines (ss : List String) : String :=
  String.join (ss.map (fun s => s ++ "\n"))

#eval ([1, 2, 3, 4] : List Nat).sum      -- 10
#eval ([1, 2, 3, 4] : List Nat).prod     -- 24
#eval ([1, 2, 3, 4] : List Nat).elem 3   -- true
#eval ([3, 1, 4, 2] : List Nat).mergeSort -- [1, 2, 3, 4]

def qsort : List Nat → List Nat
  | [] => []
  | x :: xs =>
    let smaller := xs.filter (fun y ↦ y ≤ x)
    let larger := xs.filter (fun y ↦ y > x)
    qsort smaller ++ [x] ++ qsort larger
termination_by xs => xs.length
decreasing_by
  all_goals
    simp_wf
    apply Nat.lt_succ_of_le
    calc
      _ ≤ xs.attach.length := List.length_filter_le ..
      _ = xs.length := by simp

-- LIVE: Recursion first, then express sum using foldRight.
def mapList (f : α → β) : List α → List β := sorry
def foldRight (f : α → β → β) (base : β) : List α → β := sorry
def sum (xs : List Nat) : Nat := sorry
end Course.Template03
