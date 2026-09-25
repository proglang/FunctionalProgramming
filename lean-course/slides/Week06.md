---
title: "Week 06: Effects and `do` notation"
subtitle: "Functional Programming in Lean"
date: ""
aspectratio: 169
fontsize: 17pt
---

## Learning goals

- Read `Option`, `Except` and `IO` result types
- Sequence computations with `do`
- Keep errors explicit in a function signature

## Central idea

An effectful computation returns a value together with a declared possibility such as failure.

## Lean example

```lean
def safeDivide (x y : Nat) :
    Except String Nat :=
  if y == 0 then .error "division by zero"
  else .ok (x / y)
```

## What to notice

- `Except String Nat` contains a result or an error
- `do` propagates an error without repeated matches
- `IO` marks interaction with the external world

## Live coding

1. Complete `safeDivide`
2. Write `ratioOfSums` with `do`
3. Compare success and failure with `#eval`

## Pause and predict

What does `ratioOfSums 10 2 0 0` return?

## Exercise connection

Open `exercises/Week06.md` after the lecture.

The complete example is `Course/Week06.lean`; the fill-in version is `Templates/Week06.lean`.
