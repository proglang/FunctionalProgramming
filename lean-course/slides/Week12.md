---
title: "Week 12: Lean macros"
subtitle: "Functional Programming in Lean"
date: ""
aspectratio: 169
fontsize: 17pt
---

## Learning goals

- Syntax quotations
- Antiquotations
- Term and tactic macros

## Core concepts

- Macro expansion: `Syntax` to `Syntax`
- Quotation: parsed syntax as data
- Antiquotation: captured syntax insertion

## Lean example

```lean
syntax "twice(" term ")" : term
macro_rules
  | `(twice($x)) => `($x + $x)
#eval twice(21)
```

## Lean details

- `twice`: term macro
- `close_simple`: tactic macro
- Kernel checking after elaboration

## Live coding

- `twice` expansion
- `close_simple` expansion
- Unsolved goal after tactic failure

## Check

Role of the kernel after macro expansion

## Exercise

- `thrice` macro
- A goal outside `close_simple`
