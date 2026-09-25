---
title: "Week 02: Inductive types and recursion"
subtitle: "Functional Programming in Lean"
date: ""
aspectratio: 169
fontsize: 17pt
---

## Learning goals

- `inductive` constructors
- `structure` fields
- Structural recursion on `List`

## Core concepts

- `Suit`: alternatives defined by constructors
- `Card`: a structure with two fields
- List cases: `[]` and `x :: xs`

## Lean example

```lean
inductive Suit where
  | clubs | diamonds | hearts | spades

def isRed : Suit → Bool
  | .hearts | .diamonds => true
  | .clubs | .spades => false
```

## Lean details

- Pattern matching on every constructor
- Recursive call on the list tail
- Termination from structural recursion

## Live coding

- `isRed`: cases for each suit
- `countRed`: empty and cons cases
- Evaluation on a short card list

## Check

Base case of `countRed`

## Exercise

- `countSuit` on a card list
- Termination argument
