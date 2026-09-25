---
title: "Week 13: Specification-driven programming"
subtitle: "Functional Programming in Lean"
date: ""
aspectratio: 169
fontsize: 17pt
---

## Learning goals

- Fixed formal specification
- Generated implementation and proof
- Proof dependency audit

## Core concepts

- `partitionBy`: stable order in both outputs
- `List.filter`: reference behavior
- Implementation and theorem statements as separate objects

## Lean example

```lean
theorem partitionBy_yes
    (p : α → Bool) (xs : List α) :
  (partitionBy p xs).1 = xs.filter p := by
  sorry
```

## Lean details

- Intentional `sorry` in the displayed template
- `sorryAx` in an axiom report: incomplete proof
- Human review of the specification

## Live coding

- Fixed theorem statements
- Candidate from an LLM
- Compilation and `#print axioms`

## Check

Output order under a membership specification

## Exercise

- Counterexample to `(xs, [])`
- Review of generated proof
