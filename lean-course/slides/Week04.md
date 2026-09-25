---
title: "Week 04: Trees and strict evaluation"
subtitle: "Functional Programming in Lean"
date: ""
aspectratio: 169
fontsize: 17pt
---

## Learning goals

- Recursion on an inductive tree
- Accumulator-based recursion
- Strict evaluation in Lean

## Core concepts

- Tree cases: leaf and node
- Size from recursive subtree results
- Delayed computation: `Unit → α`

## Lean example

```lean
def Tree.size : Tree α → Nat
  | .leaf => 0
  | .node left _ right =>
      left.size + 1 + right.size
```

## Lean details

- Persistent tree values
- Tail-recursive list reversal
- Termination from smaller subtrees

## Live coding

- `Tree.size`
- `reverseAcc`
- Comparison with `List.reverse`

## Check

Termination argument for `Tree.size`

## Exercise

- `Tree.height`
- Tree-map size property
