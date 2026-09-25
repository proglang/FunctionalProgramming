# Week 14 exercise: Bonus: inference constraints

**Format:** 90-minute exercise session. Work in `Templates/Week14.lean`; compare with `Course/Week14.lean` after attempting the tasks. The bonus task is optional.

## 1. Read and predict (20 minutes)

Infer the type of `fun x => x` and generate constraints for applying it to a natural number.

## 2. Program (45 minutes)

Complete `occurs` and the arrow case of `decompose`. Trace the constraints from `α → Nat = Nat → β`.

## 3. Explain or prove (25 minutes)

Show why `α = α → Nat` fails. Explain how a let-bound identity can be used at two types in Mini-ML.

## Bonus

Implement substitution propagation across a list of remaining constraints.
