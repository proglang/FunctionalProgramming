---
title: "Week 03: Polymorphism and folds"
subtitle: "Functional Programming in Lean"
date: ""
aspectratio: 169
fontsize: 17pt
---

## Learning goals

- Higher-order function arguments
- Polymorphic list functions
- `foldRight`

## Core concepts

- `mapList`: elementwise function application
- `foldRight`: replacement of list constructors
- Independent element and result types

## Lean example

```lean
def foldRight (f : α → β → β)
    (base : β) : List α → β
  | [] => base
  | x :: xs => f x (foldRight f base xs)
```

## Lean details

- Type parameters `α` and `β`
- Curried function arguments
- Recursive result as fold accumulator

## Live coding

- `mapList` by recursion
- `foldRight` by recursion
- `sum` from `foldRight`

## Check

Type of `foldRight (· + ·) 0`

## Exercise

- `length` from `foldRight`
- List identity law
