# Week 06 exercise: Errors, monads and I/O

**Format:** 90-minute exercise session. Work in `Templates/Week06.lean`; compare with `Course/Week06.lean` after attempting the tasks. The bonus task is optional.

## 1. Read and predict (20 minutes)

Trace `safeDivide 8 2` and `safeDivide 8 0`. State the type and value of each result.

## 2. Program (45 minutes)

Complete `safeDivide` and `ratioOfSums` using `do`. Add `divideAfterIncrement` that increments the numerator before division.

## 3. Explain or prove (25 minutes)

Rewrite one `do` block using an explicit match on `Except`. Explain which version repeats less error-handling code.

## Bonus

Write an `IO` entry point that reads a line and reports whether it is a natural number.
