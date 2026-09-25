---
title: "Week 15: Bonus: monad transformers"
subtitle: "Functional Programming in Lean"
date: ""
aspectratio: 169
fontsize: 17pt
---

## Learning goals

- `StateT` and `ExceptT`
- Effect-stack result types
- State after an error

## Core concepts

- `StateT Nat (Except String) A`: error or value with state
- `ExceptT String (StateM Nat) A`: error or value, plus state
- Transformer order and failure semantics

## Lean example

```lean
abbrev LoseState :=
  StateT Nat (Except String)
abbrev KeepState :=
  ExceptT String (StateM Nat)
```

## Lean details

- `failLose`: discarded final state
- `failKeep`: final state `1`
- Result type as the explanation

## Live coding

- State increment before error
- Evaluation of both stacks
- Comparison of outputs

## Check

Stack retaining state after failure

## Exercise

- Expansion of both result types
- Error trace
