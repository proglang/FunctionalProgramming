---
title: "Week 16: Bonus: categories and functors"
subtitle: "Functional Programming in Lean"
date: ""
aspectratio: 169
fontsize: 17pt
---

## Learning goals

- Identity and composition
- Category laws
- Functor laws for `Option`

## Core concepts

- Objects: types
- Morphisms: functions
- Functor mapping: types and morphisms

## Lean example

```lean
def compose (f : α → β)
    (g : β → γ) : α → γ :=
  fun x => g (f x)

theorem option_map_id (x : Option α) :
  id <$> x = x := by
  cases x <;> rfl
```

## Lean details

- Composition associativity
- `Option.map` preserves identity
- `Option.map` preserves composition

## Live coding

- `compose_assoc` with `funext`
- `option_map_id` by cases
- `option_map_comp` by cases

## Check

`Option.map` applied to `id`

## Exercise

- Composition-law proof
- Tree-map law
