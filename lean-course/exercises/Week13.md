# Week 13 exercise: Reviewing generated code

**Format:** 90-minute exercise session. Work in `Templates/Week13.lean`; compare with `Course/Week13.lean` after attempting the tasks. The bonus task is optional.

## 1. Read and predict (20 minutes)

Read both `partitionBy` theorems. Explain why they specify the order of each output list as well as its elements.

## 2. Program (45 minutes)

Consider the proposed implementation `fun _ xs => (xs, [])`. Give a concrete input on which the first theorem fails. Then complete or inspect a correct recursive implementation.

## 3. Explain or prove (25 minutes)

Inspect a proof that contains `sorry`, compile it, and run `#print axioms`. Explain why a successful command alone is insufficient. Review the final proof and the teacher-owned specification separately.

## Bonus

Strengthen the specification with an explicit statement about the combined output lengths.
