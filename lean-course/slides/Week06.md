---
title: "Week 06: `Except`, `IO`, and `do`"
subtitle: "Functional Programming in Lean"
date: ""
aspectratio: 169
fontsize: 17pt
---

## Learning goals

- Explicit error types
- Monadic sequencing with `do`
- `IO` actions

## Core concepts

- `Except String Nat`: success or error
- `do`: sequencing and error propagation
- `IO`: interaction with the external world

## Lean example

```lean
def safeDivide (x y : Nat) :
    Except String Nat :=
  if y == 0 then .error "division by zero"
  else .ok (x / y)
```

## Lean details

- `.ok` and `.error` constructors
- `←` for a monadic result
- Error handling in the return type

## Live coding

- `safeDivide`
- `ratioOfSums` in `do` notation
- Success and failure with `#eval`

## Check

Result of `ratioOfSums 10 2 0 0`

## Exercise

- `divideAfterIncrement`
- Explicit match versus `do`
