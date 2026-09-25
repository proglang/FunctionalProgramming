---
title: "Week 08: Interpreters with errors"
subtitle: "Functional Programming in Lean"
date: ""
aspectratio: 169
fontsize: 17pt
---

## Learning goals

- Inductive expression syntax
- Recursive evaluation
- Error propagation

## Core concepts

- `Expr`: syntax tree
- `eval`: meaning of each constructor
- `Except String Int`: value or error

## Lean example

```lean
def eval : Expr → Except String Int
  | .lit n => .ok n
  | .add x y => do
      let a ← eval x
      let b ← eval y
      pure (a + b)
```

## Lean details

- Left subexpression before right subexpression
- Division-by-zero branch
- New constructor, new evaluator case

## Live coding

- Division constructor
- Division evaluator case
- Nested error trace

## Check

Evaluation order of `.add x y`

## Exercise

- Multiplication constructor
- Nested error trace
