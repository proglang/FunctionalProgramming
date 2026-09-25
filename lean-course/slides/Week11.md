---
title: "Week 11: Indexed terms and typed evaluation"
subtitle: "Functional Programming in Lean"
date: ""
aspectratio: 169
fontsize: 17pt
---

## Learning goals

- Read a type indexed by a value
- Construct expressions that preserve their result type
- Explain why an ill-typed expression has no constructor

## Central idea

The index of `Expr` records the type of the value produced by evaluation.

## Lean example

```lean
inductive Expr : Ty → Type where
  | number (n : Nat) : Expr .nat
  | add (x y : Expr .nat) : Expr .nat
  | choose (c : Expr .bool)
      (x y : Expr t) : Expr t
```

## What to notice

- `denote` interprets a syntax type as a Lean type
- Pattern matching refines the result type
- An `add` node cannot take a Boolean child

## Live coding

1. Complete the indexed evaluator
2. Construct a conditional expression
3. Try to construct an invalid addition and read the error

## Pause and predict

Why does `eval` need a result type depending on `t`?

## Exercise connection

Open `exercises/Week11.md` after the lecture.

The complete example is `Course/Week11.lean`; the fill-in version is `Templates/Week11.lean`.
