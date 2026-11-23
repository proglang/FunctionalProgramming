import Std
namespace Course.NBE

inductive Ty where
  | Nat : Ty
  | Bool : Ty
  | Arrow : Ty → Ty → Ty
  | Prod : Ty → Ty → Ty

abbrev Ctx (n : Nat) := Fin n → Ty

def extend : {n : Nat} → (Fin n → α) → α → Fin (n + 1) → α
  | _, Γ, α, i =>
    match i with
    | ⟨0, _⟩ => α
    | ⟨i + 1, h⟩ => Γ ⟨i, Nat.lt_of_succ_lt_succ h⟩

inductive Tm : (n : Nat) → Ctx n → Ty → Type where
  | var : ∀ {n Γ}, (i : Fin n) → Tm n Γ (Γ i)
  | nat : ∀ {n Γ}, Nat → Tm n Γ .Nat
  | bool : ∀ {n Γ}, Bool → Tm n Γ .Bool
  | lam : ∀ {n Γ α β}, Tm (n + 1) (extend Γ α) β → Tm n Γ (.Arrow α β)
  | app : ∀ {n Γ α β}, Tm n Γ (.Arrow α β) → Tm n Γ α → Tm n Γ β
  | pair : ∀ {n Γ α β}, Tm n Γ α → Tm n Γ β → Tm n Γ (.Prod α β)
  | fst : ∀ {n Γ α β}, Tm n Γ (.Prod α β) → Tm n Γ α
  | snd : ∀ {n Γ α β}, Tm n Γ (.Prod α β) → Tm n Γ β

def 𝒯 : Ty → Type
  | .Nat => Nat
  | .Bool => Bool
  | .Arrow α β => 𝒯 α → 𝒯 β
  | .Prod α β => 𝒯 α × 𝒯 β

abbrev Env (Γ : Ctx n) := (i : Fin n) → 𝒯 (Γ i)

def ℰ : ∀ {n Γ α}, Tm n Γ α → Env Γ → 𝒯 α
  | _, _, _, Tm.var i, ρ => ρ i
  | _, _, _, Tm.nat m, _ => m
  | _, _, _, Tm.bool b, _ => b
  | _, _, _, Tm.lam t, ρ => fun x => ℰ t (extend ρ x)
  | _, _, _, Tm.app t u, ρ => (ℰ t ρ) (ℰ u ρ)
  | _, _, _, Tm.pair t u, ρ => (ℰ t ρ, ℰ u ρ)
  | _, _, _, Tm.fst t, ρ => (ℰ t ρ).1
  | _, _, _, Tm.snd t, ρ => (ℰ t ρ).2



end Course.NBE
