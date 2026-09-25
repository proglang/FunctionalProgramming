# Week 01 exercise: Lean expressions and types

**Format:** 90-minute exercise session. Work in `Templates/Week01.lean`; compare with `Course/Week01.lean` after attempting the tasks. The bonus task is optional.

## 1. Read and predict (20 minutes)

Predict the values of `double 7`, `larger 12 9`, and `larger 9 12`. Then use `#eval` to check. For each function, say what its arguments and result types are.

## 2. Program (45 minutes)

Complete `double` and `larger` in the template. Add `clamp (limit value : Nat) : Nat` that returns the smaller of its arguments. Test the boundary case where they are equal.

## 3. Explain or prove (25 minutes)

Read the error from applying `double` to a string. Explain the expected type and the actual type without changing the definition.

## Bonus

Write a theorem that `double 0 = 0` and close it with `rfl`.
