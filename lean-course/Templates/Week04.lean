import Std
namespace Course.Template04

#check List.elem
#check List.insert

def insertSorted [LE α] [DecidableLE α] (x : α) : List α → List α
  | [] => [x]
  | y :: ys =>
    if x ≤ y then
      x :: y :: ys
    else
      y :: insertSorted x ys

#check insertSorted

inductive Sorted [LE α] : List α → Prop where
  | nil : Sorted []
  | cons : ∀ {x xs}, Sorted xs → (∀ y ∈ xs, x ≤ y) → Sorted (x :: xs)

inductive Sorted' [LE α] : List α → Prop where
  | nil : Sorted' []
  | singleton : ∀ x, Sorted' [x]
  | cons : ∀ {x y xs}, x ≤ y → Sorted' (y :: xs) → Sorted' (x :: y :: xs)

def Sorted123 : Prop := Sorted [1, 2, 3]

theorem holds_Sorted123 : Sorted123 :=
  Sorted.cons (Sorted.cons (Sorted.cons (Sorted.nil)
                           (by decide))
              (by decide))
  (by decide)

def square [Mul α] (x : α) : α := x * x

#check square

#print LawfulBEq

class Monoid (α : Type) where
  op : α → α → α
  id : α

instance MonoidNat : Monoid Nat where
  op := Nat.add
  id := 0

instance MonoidInt : Monoid Int where
  op := Int.add
  id := 0

instance MonoidIntMul : Monoid Int where
  op := Int.mul
  id := 1

instance MonoidString : Monoid String where
  op := String.append
  id := ""

instance MonoidPair [Monoid α][Monoid β] : Monoid (α × β) where
  op := fun (x1, y1) (x2, y2) => (Monoid.op x1 x2, Monoid.op y1 y2)
  id := (Monoid.id, Monoid.id)

#eval Monoid.op (1, 2) (3, 4) -- should be (4, 6)
#eval Monoid.op (1, "a") (0, "b") -- should be (1, "ab")

class Group (α : Type) extends Monoid α where
  inv : α → α

instance GroupInt : Group Int where
  inv := Int.neg

instance GroupPair [Group α][Group β] : Group (α × β) where
  inv := fun (x, y) => (Group.inv x, Group.inv y)

def grp12 : (Int × Int) := (1, 2)
#eval Group.inv grp12 -- should be (-1, -2)
#eval Group.inv (-1, -2) -- should be (1, 2)

class LawfulMonoid (α : Type) [Monoid α] where
  op_assoc : ∀ x y z : α , Monoid.op (Monoid.op x y) z = Monoid.op x (Monoid.op y z)
  id_op : ∀ x : α , Monoid.op Monoid.id x = x
  op_id : ∀ x : α , Monoid.op x Monoid.id = x

class LawfulGroup (α : Type) [Group α] extends LawfulMonoid α where
  inv_op : ∀ x : α , Monoid.op (Group.inv x) x = Monoid.id
  op_inv : ∀ x : α , Monoid.op x (Group.inv x) = Monoid.id

#print Group


namespace Experiment
-- local definitions for equality and lists, to avoid conflicts with Lean's built-in definitions

inductive Eq (x : α) : α → Prop where
  | refl : Eq x x

def eq_bad : Prop := Eq 1 2
def eq_good : Prop := Eq 1 1
theorem eq_good_holds : eq_good := by
  apply Eq.refl
theorem eq_bad_does_not_hold : ¬ eq_bad := by
  intro h
  cases h

theorem symEq : ∀ {x y : α}, Eq x y → Eq y x :=
  fun h =>
  match h with
  | Eq.refl => Eq.refl


theorem transEq : ∀ {x y z : α}, Eq x y → Eq y z → Eq x z :=
  fun h1 h2 =>
  match h1, h2 with
  | Eq.refl, Eq.refl => Eq.refl

theorem symEq' : ∀ {x y : α}, Eq x y → Eq y x := by
  intro x y h
  cases h
  apply Eq.refl

theorem transEq' : ∀ {x y z : α}, Eq x y → Eq y z → Eq x z := by
  intro x y z h1 h2
  cases h1
  cases h2
  apply Eq.refl

inductive List α where
  | nil : List α
  | cons : α → List α → List α

def beq_list [BEq α] : List α → List α → Bool
  | .nil, .nil => true
  | .cons x xs, .cons y ys => BEq.beq x y && beq_list xs ys
  | _, _ => false

instance [BEq α]: BEq (List α) where
  beq xs ys := beq_list xs ys

instance [BEq α][ReflBEq α] : ReflBEq (List α) where
  rfl := by
    intro xs
    induction xs with
    | nil => rfl
    | cons x xs ih =>
      change ((x == x) && (xs == xs)) = true
      rw [beq_self_eq_true, ih]
      rfl

class ToString (α : Type) where
  toString : α → String

#print Repr

class Repr (α : Type) where
  reprPrec : α → Nat → Std.Format

#print Ord
#print Ordering

inductive And (p q : Prop) : Prop where
  | intro : (left : p) → q → And p q

inductive Or (p q : Prop) : Prop where
  | inl : p → Or p q
  | inr : q → Or p q

-- proof using tactics
theorem and_comm {p q : Prop} : And p q → And q p := by
  intro pq
  exact And.intro pq.2 pq.1

-- proof using pattern matching
theorem and_comm' {p q : Prop} : And p q → And q p
  | And.intro left right => And.intro right left

end Experiment

theorem and_comm {p q : Prop} : p ∧ q → q ∧ p :=
  fun pq =>
  match pq with
  | .intro left right => .intro right left

theorem and_comm' { p q : Prop} : p ∧ q → q ∧ p := by
  intro pq
  exact .intro pq.right pq.left

theorem and_comm'' {p q : Prop} : p ∧ q → q ∧ p := by
  intro pq
  cases pq with
  | intro left right =>
    exact .intro right left

theorem or_comm {p q : Prop} : p ∨ q → q ∨ p := sorry

theorem or_comm' {p q : Prop} : p ∨ q → q ∨ p := sorry

theorem and1 : 1 + 1 = 2 ∧ 2 + 2 = 4 := by
  decide

theorem or1 : 1 + 1 = 2 ∨ 2 + 2 = 5 := by
  decide

theorem and2 : 1 + 1 = 2 ∧ 2 + 2 ≤ 5 := by
  decide

inductive Color where
  | red
  | black

instance : ToString Color where
  toString
    | .red => "red"
    | .black => "black"

instance : BEq Color where
  beq
    | .red, .red => true
    | .black, .black => true
    | _, _ => false

theorem easyDef1 : 4 + 5 = 3 * 3 := by
  rfl

theorem easyDef2 : square 3 = 9 := by
  rfl

theorem easyDef3 : insertSorted 1 [] = [1] := by
  rfl

theorem easyDef4 : insertSorted 1 [2, 3] = [1, 2, 3] := by
  rfl

theorem easyDef5 : insertSorted 2 [1, 3] = [1, 2, 3] := by
  rfl

theorem easyDef6 :
  insertSorted 1 [2, 3] = insertSorted 2 [1, 3] := by
  rfl

def safeSub (x : Nat) (y : { z : Nat // z ≤ x }) : Nat :=
  x - y.val

#eval safeSub 5 { val := 3, property := by decide }

theorem easyDef0 : 2 * 3 = 6 := eq_of_beq (BEq.refl _)

def prop1 : Prop := 3 + 4 = 7
theorem prop1_is_true : prop1 := rfl

def prop2 : Prop := 3 + 4 = 8

def prop3 : Prop := 3 ≤ 4
theorem prop3_is_true : prop3 := by
  unfold prop3
  decide

theorem prop3_unfolded : 3 ≤ 4 := by
  decide

def guitarists : List String := ["Eddie Van Halen", "Django Reinhardt", "B.B. King"]
def keyboarders : List String := ["Jon Lord", "Keith Emerson", "Jordan Rudess"]
def bassists : List String := ["Geddy Lee", "John Myung", "Chris Squire"]

def john_myung : String := bassists[1]
#eval john_myung
def chris_squire : String := bassists[2]
#eval chris_squire
-- def morty : String := bassists[3] -- This will cause an error because the index is out of bounds
-- #eval! morty
-- not accepted:
-- def third {α } (xs : List α) : α := xs[2]

def third {α} (xs : List α) (h : xs.length > 2) : α := xs[2]

#eval third bassists (by decide)

-------------------------------------------------------
-- Some proofs about sortedness
-------------------------------------------------------

theorem holds_Sorted'_123 : Sorted' [1, 2, 3] :=
  Sorted'.cons (by decide)
    (Sorted'.cons (by decide)
      (Sorted'.singleton 3))

theorem holds_Sorted_nil : Sorted ([] : List Nat) := by
  apply Sorted.nil

theorem holds_Sorted_1 : Sorted [1] := by
  apply Sorted.cons
  . apply Sorted.nil
  . intro y y_in
    cases y_in

theorem holds_Sorted_12 : Sorted [1, 2] := by
  apply Sorted.cons
  . apply Sorted.cons
    . apply Sorted.nil
    . intro y y_in
      cases y_in
  . intro y y_in
    cases y_in with
    | head => decide
    | tail => rename_i h
              cases h

theorem holds_Sorted'_nil : Sorted' ([] : List Nat) := by
  apply Sorted'.nil

theorem holds_Sorted'_1 : Sorted' [1] := by
  apply Sorted'.singleton

theorem holds_Sorted'_12 : Sorted' [1, 2] := by
  apply Sorted'.cons
  . decide
  . apply Sorted'.singleton

theorem holds_Sorted'_123' : Sorted' [1, 2, 3] := by
  apply Sorted'.cons
  . decide
  . apply Sorted'.cons
    . decide
    . apply Sorted'.singleton

theorem insert_preserves_Nat :
  ∀ (x : Nat) (xs : List Nat),
  Sorted' xs → Sorted' (insertSorted x xs) := by
  intro x xs sorted_xs
  induction sorted_xs with
  | nil =>
    unfold insertSorted
    apply Sorted'.singleton
  | singleton y =>
    unfold insertSorted
    by_cases hxy : x ≤ y
    . -- Case: x ≤ y
      rw [ite_eq_left hxy]
      apply Sorted'.cons
      . assumption
      . apply Sorted'.singleton
    . -- Case: ¬(x ≤ y)
      rw [ite_eq_right hxy]
      apply Sorted'.cons
      . exact (Nat.le_of_not_le hxy)
      . apply Sorted'.singleton
  | cons x_le_y sorted_rest ih =>
    unfold insertSorted
    rename_i a b bs
    by_cases hxa : x ≤ a
    . -- Case: x ≤ a
      rw [ite_eq_left hxa]
      constructor
      · exact hxa
      · constructor
        · exact x_le_y
        · exact sorted_rest
    . -- Case: ¬(x ≤ a)
      rw [ite_eq_right hxa]
      by_cases hxb : x ≤ b
      . -- Case: x ≤ b
        simp only [insertSorted, ite_eq_left hxb] at ih ⊢
        constructor
        · exact (Nat.le_of_not_le hxa)
        · exact ih
      . -- Case: ¬(x ≤ b)
        simp only [insertSorted, ite_eq_right hxb] at ih ⊢
        constructor
        · exact x_le_y
        · exact ih

theorem insert_preserves :
  ∀ [LE α] [DecidableLE α] (_ : ∀ a b : α, a ≤ b ∨ b ≤ a) (x : α) (xs : List α),
  Sorted' xs → Sorted' (insertSorted x xs) := by
  intro α decLE total x xs h
  induction h with
  | nil =>
    unfold insertSorted
    apply Sorted'.singleton
  | singleton y =>
    unfold insertSorted
    by_cases hxy : x ≤ y
    . -- Case: x ≤ y
      rw [ite_eq_left hxy]
      apply Sorted'.cons
      . exact hxy
      . apply Sorted'.singleton
    . -- Case: ¬(x ≤ y)
      rw [ite_eq_right hxy]
      apply Sorted'.cons
      . exact (total x y).resolve_left hxy
      . apply Sorted'.singleton
  | cons x_le_y sorted_rest ih =>
    unfold insertSorted
    rename_i a b bs
    by_cases hxa : x ≤ a
    . -- Case: x ≤ a
      rw [ite_eq_left hxa]
      constructor
      · exact hxa
      · constructor
        · exact x_le_y
        · exact sorted_rest
    . -- Case: ¬(x ≤ a)
      rw [ite_eq_right hxa]
      by_cases hxb : x ≤ b
      . -- Case: x ≤ b
        simp only [insertSorted, ite_eq_left hxb] at ih ⊢
        constructor
        · exact (total x a).resolve_left hxa
        · exact ih
      . -- Case: ¬(x ≤ b)
        simp only [insertSorted, ite_eq_right hxb] at ih ⊢
        constructor
        · exact x_le_y
        · exact ih


end Course.Template04
