---
title: "Week 09: Specifications and induction"
subtitle: "Functional Programming in Lean"
date: ""
aspectratio: 169
fontsize: 17pt
---

## Learning goals

- Theorem statements as specifications
- Induction on a list
- Axiom audit

## Core concepts

- Base case: empty list
- Cons case: head and induction hypothesis
- Goal simplification with definitions

## Lean example

```lean
theorem append_nil (xs : List α) :
    append xs [] = xs := by
  induction xs with
  | nil => rfl
  | cons x xs ih => simp [append, ih]
```

## Lean details

- `ih`: property of the tail
- `simp [append, ih]`: definition and hypothesis
- `#print axioms`: transitive axiom dependencies

## Live coding

- Goals after `induction xs`
- Cons-case proof
- Axiom report

## Check

Statement of `ih` in the cons case

## Exercise

- `append [] ys = ys`
- Associativity of `append`
