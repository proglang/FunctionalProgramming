# Week 11 exercise: Indexed syntax

**Format:** 90-minute exercise session. Work in `Templates/Week11.lean`; compare with `Course/Week11.lean` after attempting the tasks. The bonus task is optional.

## 1. Read and predict (20 minutes)

For each constructor of `Expr`, identify its input indices and result index.

## 2. Program (45 minutes)

Complete `eval`. Construct a Boolean expression and a natural-number conditional expression. Ask Lean to reject an addition with a Boolean operand.

## 3. Explain or prove (25 minutes)

Explain why `denote .nat` and `denote .bool` produce different result types. Read the rejected expression’s error message.

## Bonus

Add multiplication at index `.nat` and update `eval`.
