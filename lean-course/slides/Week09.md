---
title: "Week 09: Specifications and induction"
subtitle: "Functional Programming in Lean"
date: ""
aspectratio: 169
fontsize: 17pt
---

## Learning goals

- Read a theorem as a specification
- Follow the base and induction cases
- Audit which axioms a proof uses

## Central idea

A recursive function suggests the induction structure for a property about it.

## Lean example

```lean
theorem append_nil (xs : List α) :
    append xs [] = xs := by
  induction xs with
  | nil => rfl
  | cons x xs ih => simp [append, ih]
```

## What to notice

- The induction hypothesis speaks about the tail
- `simp` uses definitions and known equalities
- `#print axioms` exposes proof dependencies

## Live coding

1. Read the goal after `induction xs`
2. Complete the cons case
3. Explain the final theorem in English

## Pause and predict

What exactly does `ih` state in the cons case?

## Exercise connection

Open `exercises/Week09.md` after the lecture.

The complete example is `Course/Week09.lean`; the fill-in version is `Templates/Week09.lean`.
