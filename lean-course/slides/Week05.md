---
title: "Week 05: Type classes and instances"
subtitle: "Functional Programming in Lean"
date: ""
aspectratio: 169
fontsize: 17pt
---

## Learning goals

- `class` declarations
- `instance` declarations
- Instance synthesis

## Core concepts

- `Label α`: overloaded operation
- `[Label α]`: class parameter
- `instance`: implementation for a type

## Lean example

```lean
class Label (α : Type) where
  label : α → String

def describe [Label α] (x : α) : String :=
  Label.label x
```

## Lean details

- Type class constraint at `describe`
- Instance synthesis at each call site
- Missing-instance diagnostic

## Live coding

- `Label TrafficLight` instance
- `Label Nat` instance
- `#check describe`

## Check

Instance used by `describe TrafficLight.green`

## Exercise

- Instance for a second type
- Missing-instance diagnosis
