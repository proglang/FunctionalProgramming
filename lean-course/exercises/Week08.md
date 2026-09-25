# Week 08 exercise: Interpreters

**Format:** 90-minute exercise session. Work in `Templates/Week08.lean`; compare with `Course/Week08.lean` after attempting the tasks. The bonus task is optional.

## 1. Read and predict (20 minutes)

Draw the AST for `(4 + 5) / 3`. Predict the result of evaluating it.

## 2. Program (45 minutes)

Complete `eval`. Add multiplication and a corresponding evaluator case. Test nested successful and failing expressions.

## 3. Explain or prove (25 minutes)

Trace the order in which subexpressions are visited. Show where `Except.error` propagates to the outer call.

## Bonus

Add a variable constructor and evaluate with a small environment.
