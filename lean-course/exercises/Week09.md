# Week 09 exercise: Proof reading and induction

**Format:** 90-minute exercise session. Work in `Templates/Week09.lean`; compare with `Course/Week09.lean` after attempting the tasks. The bonus task is optional.

## 1. Read and predict (20 minutes)

Read `append_nil` in the solution. State the theorem in ordinary language and identify the base and cons cases.

## 2. Program (45 minutes)

Complete the template proof. Then prove `append [] ys = ys` and compare why that proof is shorter.

## 3. Explain or prove (25 minutes)

In the cons case, write the exact induction hypothesis. Explain what `simp [append, ih]` changes in the goal. Run `#print axioms` on both theorems.

## Bonus

Prove associativity of the custom `append`.
