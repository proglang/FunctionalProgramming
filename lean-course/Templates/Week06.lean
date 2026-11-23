import Std
namespace Course.Template06

inductive Op where
  | add : Op
  | sub : Op
  | mul : Op
  | div : Op
  deriving Repr, DecidableEq

instance : ToString Op where
  toString
    | .add => "+"
    | .sub => "-"
    | .mul => "*"
    | .div => "/"

inductive Term where
  | con : Nat → Term
  | bin : Term → Op → Term → Term
  deriving Repr, DecidableEq

def opEval : Op → Nat → Nat → Nat
  | .add, x, y => x + y
  | .sub, x, y => x - y
  | .mul, x, y => x * y
  | .div, x, y => x / y

def eval : Term → Nat
  | .con n => n
  | .bin t1 op t2 => opEval op (eval t1) (eval t2)

-- error handling
inductive Exc α where
  | return : α → Exc α
  | raise : String → Exc α

abbrev Exception' α := String ⊕ α

def evalExc : Term → Exc Nat
  | .con n => Exc.return n
  | .bin t1 op t2 =>
    match evalExc t1, evalExc t2 with
    | Exc.raise msg, _ => Exc.raise msg
    | _, Exc.raise msg => Exc.raise msg
    | Exc.return v1, Exc.return v2 =>
      if op == .div && v2 == 0 then
        Exc.raise "division by zero"
      else
        Exc.return (opEval op v1 v2)

-- tracing

structure Trace α where
  trace ::
  val : α
  log : String
open Trace
def trace' : α → String → Trace α
  | val, log => { val := val, log := log }
def trace'' : α → String → Trace α
  | val, log => ⟨ val, log ⟩

def evalTrace : Term → Trace Nat
  | .con n => trace n s!"con {n}"
  | .bin t1 op t2 =>
    let r1 := evalTrace t1
    let r2 := evalTrace t2
    let v := opEval op r1.val r2.val
    trace v s!"evaluated {r1.val} {op} {r2.val} = {v}"

-- step counting

structure Count α where
  count ::
  exCount : Nat → α × Nat
open Count

def tick : Count Unit := ⟨ fun n => ((), n+1) ⟩

def evalCount : Term → Count Nat
  | .con n => ⟨ fun c => (n, c) ⟩
  | .bin t1 op t2 =>
    let r1 := evalCount t1
    let r2 := evalCount t2
    ⟨ fun c =>
      let (v1, c1) := r1.exCount c
      let (v2, c2) := r2.exCount c1
      let v := opEval op v1 v2
      (v, c2 + 1) ⟩

#print Monad
namespace Experiment
class Monad (m : Type → Type) extends Pure m, Bind m
-- where
--  map' (f : α → β) (x : m α) := x >>= fun a => pure (f a)
end Experiment

-- blank monadic evaluator
def evalM [Monad m] : Term → m Nat
  | .con n => pure n
  | .bin t1 op t2 =>
    evalM t1 >>= fun v1 =>
    evalM t2 >>= fun v2 =>
    pure (opEval op v1 v2)

def evalM' [Monad m] : Term → m Nat
  | .con n => pure n
  | .bin t1 op t2 => do
    let v1 ← evalM' t1
    let v2 ← evalM' t2
    pure (opEval op v1 v2)

abbrev Id (α :Type) := α

instance : Monad Id where
  pure x := x
  bind x f := f x

instance : Monad Exc where
  pure x := Exc.return x
  bind x f :=
    match x with
    | Exc.return v => f v
    | Exc.raise msg => Exc.raise msg

def evalExcM : Term → Exc Nat
  | .con n => pure n
  | .bin t1 op t2 => do
    let v1 ← evalExcM t1
    let v2 ← evalExcM t2
    if op == .div && v2 == 0 then
      Exc.raise "division by zero"
    else
      pure (opEval op v1 v2)

instance : Monad Trace where
  pure x := trace x ""
  bind x f :=
    let r := f x.val
    trace r.val (x.log ++ r.log)

def output (msg : String) : Trace Unit :=
  trace () msg

def evalTraceM : Term → Trace Nat
  | .con n => do
    output s!"con {n}"
    pure n
  | .bin t1 op t2 => do
    let v1 ← evalTraceM t1
    let v2 ← evalTraceM t2
    let v := opEval op v1 v2
    output s!"evaluated {v1} {op} {v2} = {v}"
    pure v

-- counting

instance : Monad Count where
  pure x := ⟨ fun c => (x, c) ⟩
  bind x f :=
    ⟨ fun c =>
      let (v1, c1) := x.exCount c
      let (v2, c2) := (f v1).exCount c1
      (v2, c2 + 1) ⟩

def evalCountM : Term → Count Nat
  | .con n => pure n
  | .bin t1 op t2 => do
    let v1 ← evalCountM t1
    let v2 ← evalCountM t2
    tick
    pure (opEval op v1 v2)

-- standard monads

-- could be imported from mathlib, but we define them here for completeness
class Semigroup (α : Type) where
  op : α → α → α

class Monoid (α : Type) extends Semigroup α where
  id : α

-- Output monad
structure Output (β : Type) (α : Type)  where
  out ::
  log : β
  val : α
open Output



-- Reader monad
structure Reader (ρ : Type) (α : Type) where
  reader ::
  run : ρ → α
open Reader


-- State monad
structure State (σ : Type) (α : Type) where
  state ::
  run : σ → σ × α
open State

-- count is a special case of the state monad, where the state is a natural number

#print LawfulMonad
#print LawfulMonad.pure_bind
#print LawfulMonad.bind_assoc
#print LawfulMonad.bind_pure_comp
#print LawfulFunctor.id_map
#print LawfulFunctor.comp_map

instance : LawfulMonad Id :=
  LawfulMonad.mk' Id
    (id_map := fun x => rfl)
    (pure_bind := fun x f => rfl)
    (bind_assoc := fun x f g => rfl)

theorem id_map_Id {α : Type _} (x : Id α) : id <$> x = x := by
  unfold Functor.map; rfl

theorem id_map_Exc {α : Type _} (x : Exc α) : id <$> x = x := by
  unfold Functor.map; cases x <;> rfl

instance : LawfulMonad Exc :=
  LawfulMonad.mk' Exc
    (id_map := id_map_Exc)
    (pure_bind := fun x f => rfl)
    (bind_assoc := fun x f g =>
      match x with
      | Exc.return v => rfl
      | Exc.raise msg => rfl)

theorem id_map_Trace {α : Type _} (x : Trace α) : id <$> x = x := by
  cases x with
  | trace val log =>
    change Trace.trace val (log ++ "") = Trace.trace val log
    simp [String.append_empty]

theorem bind_assoc_Trace {α β γ : Type _} (x : Trace α) (f : α → Trace β) (g : β → Trace γ) :
  (x >>= f) >>= g = x >>= fun a => f a >>= g := by
  cases x with
  | trace val log =>
    change
      Trace.trace (g (f val).val).val
        ((log ++ (f val).log) ++ (g (f val).val).log) =
      Trace.trace (g (f val).val).val
        (log ++ ((f val).log ++ (g (f val).val).log))
    simp [String.append_assoc]


instance : LawfulMonad Trace :=
  LawfulMonad.mk' Trace
    (id_map := id_map_Trace)
    (pure_bind := fun x f => by
      unfold Bind.bind; unfold Pure.pure; rfl)
    (bind_assoc := bind_assoc_Trace)

namespace Experiment

-- options
inductive Option (α : Type) where
  | none : Option α
  | some : α → Option α

instance : Monad Option where
  pure x := Option.some x
  bind x f :=
    match x with
    | Option.none => Option.none
    | Option.some v => f v

-- lists
inductive List (α : Type) where
  | nil : List α
  | cons : α → List α → List α

def listAppend : List α → List α → List α
  | List.nil, ys => ys
  | List.cons x xs, ys => List.cons x (listAppend xs ys)

def listBind : List α → (α → List β) → List β
  | List.nil, _ => List.nil
  | List.cons v vs, f => listAppend (f v) (listBind vs f)

instance : Monad List where
  pure x := List.cons x List.nil
  bind x f := listBind x f

end Experiment

end Course.Template06
