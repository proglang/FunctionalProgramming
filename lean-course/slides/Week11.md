---
title: "Week 11: Indexed expressions"
subtitle: "Functional Programming in Lean"
date: ""
aspectratio: 169
fontsize: 17pt
---

## Learning goals

- Indexed inductive families
- Type interpretation with `denote`
- Typed evaluation

## Core concepts

- `Expr t`: result type recorded by index
- `denote .nat = Nat`
- `denote .bool = Bool`

## Lean example

```lean
inductive Expr : Ty → Type where
  | number (n : Nat) : Expr .nat
  | add (x y : Expr .nat) : Expr .nat
  | choose (c : Expr .bool)
      (x y : Expr t) : Expr t
```

## Lean details

- `add`: two natural-number operands
- `choose`: branches at one index
- Rejected ill-typed constructor application

## Live coding

- `eval` by pattern matching
- Typed conditional expression
- Rejected Boolean addition

## Check

Result type of `eval .nat e`

## Exercise

- Boolean expression
- Multiplication constructor
