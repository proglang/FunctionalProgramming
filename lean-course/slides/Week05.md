---
title: "Week 05: Type classes and instances"
subtitle: "Functional Programming in Lean"
date: ""
aspectratio: 169
fontsize: 17pt
---

## Learning goals

- Read a polymorphic class constraint
- Define one class and two instances
- Trace instance selection at a call site

## Central idea

A class describes an operation available for several types.

## Lean example

```lean
class Label (α : Type) where
  label : α → String

def describe [Label α] (x : α) : String :=
  Label.label x
```

## What to notice

- A class is an interface of operations
- An instance supplies those operations for a type
- The argument in square brackets is resolved by instance search

## Live coding

1. Write the `TrafficLight` instance
2. Call `describe` on a light and a natural number
3. Inspect the inferred type with `#check`

## Pause and predict

Where does Lean obtain `Label TrafficLight`?

## Exercise connection

Open `exercises/Week05.md` after the lecture.

The complete example is `Course/Week05.lean`; the fill-in version is `Templates/Week05.lean`.
