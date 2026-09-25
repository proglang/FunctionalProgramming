# Week 03 exercise: Polymorphism and folds

**Format:** 90-minute exercise session. Work in `Templates/Week03.lean`; compare with `Course/Week03.lean` after attempting the tasks. The bonus task is optional.

## 1. Read and predict (20 minutes)

Instantiate the type of `mapList` for a function `Nat → String`. Give the result type.

## 2. Program (45 minutes)

Complete `mapList` and `foldRight`. Define `length` with `foldRight`, then compare it with a recursive definition.

## 3. Explain or prove (25 minutes)

Predict `foldRight (fun x acc => x :: acc) [] [1,2,3]`. Explain why changing the combining function can reverse the list.

## Bonus

Prove `mapList id xs = xs` by induction.
