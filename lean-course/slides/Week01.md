---
title: "Week 01: Functions and types"
subtitle: "Functional Programming in Lean"
date: ""
aspectratio: 169
fontsize: 17pt
---

## Learning goals

- Function definitions and type signatures
- `#eval` and `#check`
- Pure expressions

## Core concepts

- Input types and result type
- Expression evaluation without side effects

## Lean example

```lean
def double (n : Nat) : Nat := n + n
#eval double 7
#check double
```

## Lean details

- `#eval`: evaluate an expression
- `#check`: display an expression’s type
- Type errors: expected type and actual type

## Live coding

- `double`: addition on `Nat`
- `larger`: conditional expression
- Predicted results before `#eval`

## Check

Inferred type of `fun n : Nat => n + 1`

## Exercise

- `clamp` on `Nat`
- Type error diagnosis
