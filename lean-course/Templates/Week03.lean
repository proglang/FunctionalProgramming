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

-- Nat.lt_succ_of_le : ∀ {m n : Nat}, m ≤ n → m < n + 1
-- one could define m < n as m + 1 ≤ n, but Lean has a built-in definition of < that is more efficient.

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

def monoListFilter : (Nat → Bool) → List Nat → List Nat :=
  @List.filter Nat

-- LIVE: Recursion first, then express sum using foldRight.
def mapList1 : ∀ {α β}, (f : α → β) → List α → List β := sorry
def mapList2 {α β} : (f : α → β) → List α → List β := sorry
def foldRight (f : α → β → β) (z : β) : List α → β
  | [] => z
  | x :: xs => f x (foldRight f z xs)

#check foldRight

def foldRight' {α β} : (α → β → β) → (z : β) → List α → β
  | _, z, [] => z
  | f, z, x :: xs => f x (foldRight' f z xs)

def foldRight'' {α β} : (α → β → β) → β → List α → β :=
  fun f z xs =>
  match xs with
  | [] => z
  | x :: xs => f x (foldRight'' f z xs)

def concat : List (List α) → List α :=
  foldRight (· ++ ·) []
def maximum : List Nat → Nat :=
  foldRight Nat.max 0

def reverse : List α → List α :=
  foldRight (fun x rev_xs => rev_xs ++ [x]) []

def quick_reverse : List α → List α → List α
  | [], acc => acc
  | x :: xs, acc => quick_reverse xs (x :: acc)

def sum : (xs : List Nat) → Nat
  | [] => 0
  | x :: xs => x + sum xs

def sum' : (xs : List Nat) → Nat :=
  foldRight (· + ·) 0

def sum'' (xs : List Nat) : Nat :=
  foldRight (· + ·) 0 xs

def prod : (xs : List Nat) → Nat
  | [] => 1
  | x :: xs => x * prod xs

def prod' : (xs : List Nat) → Nat :=
  foldRight (· * ·) 1

--
def even (n : Nat) : Bool := n % 2 == 0
#eval List.map even [1, 2, 3, 4]
#eval List.filter even [1, 2, 3, 4]
#check List.filter (α := Int)
#check @List.filter Int
#check 'x'

abbrev T1 := Int -> Int -> Int
abbrev T2 := Int × Int -> Int

def pick : ∀ {α}, (b: Bool) → α × α → α :=
  fun b =>
  match b with
  | true => Prod.fst
  | false => Prod.snd

def pick' : ∀ {α}, (b: Bool) → α × α → α :=
  fun b p =>
  match b with
  | true => p.1
  | false => p.2

def or_list : List Bool → Bool := sorry
def and_list : List Bool → Bool := sorry
def concat : List (List α) → List α := sorry
def maximum : List Nat → Nat := sorry

#check List.take
#check List.take 5

#check foldRight
#check foldRight (· + ·)
#check foldRight (· * ·) 1

#check Char.isWhitespace

def removeWhitespace (s : String) : String :=
  (s.toList.filter (not ∘ Char.isWhitespace)).toString

def compose {α β γ : Type} (f : β → γ) (g : α → β) : α → γ :=
  fun x => f (g x)

infixr :90 " ∘ " => compose

def snoc (x : α) (xs : List α) : List α := xs ++ [x]

def snoc' : α → List α → List α :=
  fun x xs => xs ++ [x]

def f3 : List α → List α := foldRight snoc []
def f3' : List α → List α := foldRight (fun x xs => xs ++ [x]) []
def f3'' : List α → List α := foldRight (fun x => (· ++ [x])) []

end Course.Template03
