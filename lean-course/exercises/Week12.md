# Week 12 exercise: Macros and tactics

**Format:** 90-minute exercise session. Work in `Templates/Week12.lean`; compare with `Course/Week12.lean` after attempting the tasks. The bonus task is optional.

## 1. Read and predict (20 minutes)

Read the `twice` macro. Identify the quotation and antiquotation and predict the expansion of `twice(21)`.

## 2. Program (45 minutes)

Complete `twice` and `close_simple`. Use the tactic on a reflexive equality and on `n + 0 = n`.

## 3. Explain or prove (25 minutes)

Explain which part creates syntax and which part checks the resulting expression or proof. Show a claim that `close_simple` cannot prove.

## Bonus

Write a macro `thrice(x)` using the same pattern and discuss repeated evaluation.
