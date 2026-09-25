---
title: "Week 12: Lean metaprogramming"
subtitle: "Functional Programming in Lean"
date: ""
aspectratio: 169
fontsize: 17pt
---

## Learning goals

- Separate syntax from elaborated expressions
- Read a quotation and an antiquotation
- Define a small macro for terms and tactics

## Central idea

A macro transforms parsed syntax into new syntax that Lean then elaborates.

## Lean example

```lean
syntax "twice(" term ")" : term
macro_rules
  | `(twice($x)) => `($x + $x)
#eval twice(21)
```

## What to notice

- Quotation produces syntax data
- `$x` inserts captured syntax into a quotation
- The kernel still checks the generated term or proof

## Live coding

1. Complete the `twice` macro
2. Add a `close_simple` tactic macro
3. Inspect one successful and one failed expansion

## Pause and predict

Does a macro itself prove the theorem it generates?

## Exercise connection

Open `exercises/Week12.md` after the lecture.

The complete example is `Course/Week12.lean`; the fill-in version is `Templates/Week12.lean`.
