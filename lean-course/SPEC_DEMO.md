# Week 13: specification-driven programming demonstration

## Teaching aim

Students distinguish the intended requirement, its Lean specification, the proposed implementation, and the proof. A successful Lean check addresses the formal statement under its assumptions. The instructor and students still need to judge whether the statement captures the requirement.

## Requirement

Split a list according to a Boolean predicate. Keep the original order among values in each output list. The first list contains exactly the values that satisfy the predicate; the second contains the rest.

`Templates/Week13.lean` gives the fixed signatures. The two theorems equate each output with `List.filter`, which specifies order and multiplicity as well as membership. Do not allow the generated answer to change the signatures or theorem statements.

## Prompt for the live LLM

> Fill the `sorry` holes in the Lean 4.33.1 file below. Preserve all declaration names, signatures and theorem statements. Use Lean's bundled libraries only. Do not add axioms, `sorry`, `admit`, `partial`, or `unsafe`. Return a complete Lean file and a short explanation of the induction cases.

Paste the exact contents of `Templates/Week13.lean` below the prompt. In the live demonstration, make the model's response a new candidate file. Do not overwrite the template.

## Classroom sequence, about 45 minutes

1. **Before the model response:** ask whether the two theorem statements express the English requirement. Ask why a membership-only statement would leave output order unspecified.
2. **Inspect the proposed code:** find the recursive call, the predicate branch and the order in which heads are added. Use the input `[1,2,3,4,5]` with `fun n => n % 2 == 0` as a first test.
3. **Inspect the proposed proof:** identify the base case, cons case and any tactics students do not recognize. Look for changed statements and new assumptions.
4. **Run Lean:** compile the candidate. Then run `#print axioms partitionBy_yes` and `#print axioms partitionBy_no`. A report containing `sorryAx` marks an incomplete proof. Consult the Lean reference's proof-validation guidance for other axiom dependencies.
5. **Compare with the checked version:** `Course/Week13.lean` is the rehearsed fallback and reference solution.

A deliberately bad proposal is `def partitionBy (_p : α → Bool) (xs : List α) := (xs, [])`. For the even predicate and input `[1,2]`, its first output is `[1,2]` rather than `[2]`. This supplies a concrete counterexample before any proof attempt.

## Offline exam adaptation

Provide a specification, a short candidate program, and a proof fragment. Ask students to explain what the specification rules out, identify the relevant recursive branch, or spot a `sorry`-based dependency. They need no LLM or internet access.
