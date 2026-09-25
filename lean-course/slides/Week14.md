---
title: "Week 14: Bonus: type inference"
subtitle: "Functional Programming in Lean"
date: ""
aspectratio: 169
fontsize: 17pt
---

## Learning goals

- Type constraints
- Constraint decomposition for simple types
- Occurs check

## Core concepts

- Type variables and arrow types
- Arrow equality: input and output constraints
- Let-polymorphism: generalization and instantiation

## Lean example

```lean
inductive Ty where
  | nat
  | variable (id : Nat)
  | arrow (input output : Ty)
```

## Lean details

- `α → Nat = Nat → β`: two constraints
- `α = α → Nat`: occurs-check failure
- Toy constraint solver versus Lean elaborator

## Live coding

- Constraints for identity
- `occurs`
- Arrow-type decomposition

## Check

Occurs-check result for `α = α → Nat`

## Exercise

- Constraint decomposition
- Let-polymorphism example
