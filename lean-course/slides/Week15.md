---
title: "Week 15: Bonus: monad transformers"
subtitle: "Functional Programming in Lean"
date: ""
aspectratio: 169
fontsize: 17pt
---

## Learning goals

- Read a transformer stack as a result type
- Compare two orders of state and error
- Predict whether state survives failure

## Central idea

The order of effects changes observable results.

## Lean example

```lean
abbrev LoseState :=
  StateT Nat (Except String)
abbrev KeepState :=
  ExceptT String (StateM Nat)
```

## What to notice

- The first stack may lose the final state
- The second returns state alongside an error
- A shared evaluator can use an effect interface

## Live coding

1. Complete both failing computations
2. Run each from state zero
3. Explain the different outputs by expanding the types

## Pause and predict

Which stack can return a final state after an error?

## Exercise connection

Open `exercises/Week15.md` after the lecture.

The complete example is `Course/Week15.lean`; the fill-in version is `Templates/Week15.lean`.
