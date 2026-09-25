---
title: "Week 14: Bonus: type inference"
subtitle: "Functional Programming in Lean"
date: ""
aspectratio: 169
fontsize: 17pt
---

## Learning goals

- Generate constraints from simple terms
- Decompose an arrow-type equation
- Reject a cyclic type with an occurs check

## Central idea

Unification solves equality constraints on types. Let-polymorphism adds generalization.

## Lean example

```lean
inductive Ty where
  | nat
  | variable (id : Nat)
  | arrow (input output : Ty)
```

## What to notice

- `α → Nat = Nat → β` yields two smaller equations
- `α = α → Nat` fails the occurs check
- Lean elaboration solves a richer problem than this teaching model

## Live coding

1. Trace constraints for identity
2. Complete `occurs`
3. Decompose an arrow equality

## Pause and predict

Why can `α` not stand for `α → Nat`?

## Exercise connection

Open `exercises/Week14.md` after the lecture.

The complete example is `Course/Week14.lean`; the fill-in version is `Templates/Week14.lean`.
