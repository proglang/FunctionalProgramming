---
title: "Week 10: Lambda terms and evaluation"
subtitle: "Functional Programming in Lean"
date: ""
aspectratio: 169
fontsize: 17pt
---

## Learning goals

- Inductive lambda-term syntax
- De Bruijn indices
- Call-by-value evaluation with fuel

## Core concepts

- Variable, abstraction, application
- Index zero: nearest binder
- Closure: body and defining environment

## Lean example

```lean
inductive Term where
  | var (index : Nat)
  | lam (body : Term)
  | app (function argument : Term)
```

## Call-by-value evaluator

```lean
def evalCBV :
    Nat → List Value → Term → Option Value
```

- Closure: body and environment
- Fuel: evaluation bound

## Lean details

- `wellScoped`: bound-variable check
- `evalCBV`: environment-based evaluator
- Call-by-value versus call-by-name traces

## Live coding

- Identity term
- `wellScoped`
- `evalCBV` on self-application

## Check

`wellScoped 0 (.var 0)`

## Exercise

- De Bruijn translations
- Evaluation trace
