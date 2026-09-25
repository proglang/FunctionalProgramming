# Instructor guide

## Course shape

Students arrive with Python, C++ and prior proof experience, but no functional language. Introduce Lean as a programming language first. Use very small proofs early so that week 9 develops an existing habit rather than introducing a new activity. From week 9 onward, ask students to explain what a theorem states before asking how its proof works.

A default 90-minute lecture rhythm is 10 minutes of recap and prediction, 20 minutes of slides and discussion, 40 minutes of live coding in `Templates/WeekXX.lean`, 15 minutes of proof or concept questions, and 5 minutes of summary. The separate exercise sheet is planned for 20 minutes of prediction, 45 minutes of programming, and 25 minutes of explanation or proof. Adjust the live segment when a topic needs more board work.

The provided decks have eight slides each, including the title. They are prompts for a coding lecture, not a transcript of 90 minutes. Before teaching, open both the template and completed code, and rehearse the live changes under the pinned toolchain.

## Weekly sequence

| Week | Lecture focus and live work | Existing Haskell source to consult | Companion reading |
|---|---|---|---|
| 01 | Use `#eval` and `#check`; complete two small functions | `slides/00-intro.tex`, `01-starting-haskell.tex` | FP in Lean, introduction and chapter 1 |
| 02 | Model cards; recurse over a list of cards | `02-haskell-functions.tex`, `04-define-types.tex` | FP in Lean, chapter 1 |
| 03 | Build `mapList` and `foldRight` | `03-haskell-types.tex`, `05-more-about-lists.tex`, `06-higher-order.tex` | FP in Lean, chapter 1 |
| 04 | Recurse over a tree; compare reversal styles and strict evaluation | `07-laziness.tex`, `code2024/src/V20241105.hs` | FP in Lean, chapter 8 |
| 05 | Define a class and instances; read instance-search errors | `08-type-classes.tex` | FP in Lean, chapter 3 |
| 06 | Use `Except` and `do`; show a small `IO` action | `09-io.tex` | FP in Lean, chapters 2 and 4 |
| 07 | Compose parsers while tracking remaining input | `11-parsing.tex`, `12-functors-applicatives.tex` | FP in Lean, chapter 5 |
| 08 | Add division and errors to an expression interpreter | `11-monadic-interpreter.tex`, `code2024/src/V20241126.hs` | FP in Lean, chapter 4 |
| 09 | Read and complete a list induction proof; audit axioms | `01-starting-haskell.tex`, `10-test-data-generators.tex` | FP in Lean, proof interludes |
| 10 | Represent lambda terms; trace binding and evaluation | `15-lambda-calculus.tex`, `16-evaluation-strategies.tex` | FP in Lean, chapter 7 as follow-up |
| 11 | Construct indexed expressions and a typed evaluator | `14-gadt.tex`, `code2024/src/V20241217.hs` | FP in Lean, chapter 7 |
| 12 | Write a term macro and a tactic macro | new Lean material | Lean reference, macros and custom tactics |
| 13 | Run the teacher-owned specification and LLM demo | new Lean material; see `SPEC_DEMO.md` | Lean reference, validating proofs |
| 14 bonus | Decompose type constraints and explain the occurs check | `17-polymorphic-types.tex`, `code2024/src/V20250121.md` | Lean reference, elaboration |
| 15 bonus | Compare `StateT` and `ExceptT` order | `13-monad-transformers.tex`, `code2024/src/V20241210.hs` | FP in Lean, chapter 6 |
| 16 bonus | State category and functor laws; prove `Option.map` laws | `12-functors-applicatives.tex`, `code2024/src/V12.hs` | FP in Lean, chapter 5 |

The `slides/` paths in this table refer to the original repository directory, not this package's `slides/` directory.

## Teaching decisions

- **Strictness:** Week 4 contrasts Lean's strict evaluation with Haskell's lazy evaluation. Use finite lists for the main code. An explicit `Unit → α` function illustrates delayed work. Infinite lazy lists and the sieve remain comparative examples, not code to port literally.
- **Proofs:** Week 9 proves properties of custom list functions. The solution code includes `#print axioms` so students see that a theorem's dependencies matter. Weeks 11–13 apply that habit to typed programs and generated proofs.
- **Lambda calculus:** Week 10 uses De Bruijn indices to expose binding without implying that capture-avoiding substitution is easy. The live code checks well-scoped terms; reductions are traced on the board. This keeps the 90-minute target realistic.
- **Metaprogramming:** Week 12 shows syntax quotation and a tactic macro. A macro produces syntax; elaboration and kernel checking remain distinct stages. Avoid presenting the small `close_simple` tactic as a general proof search procedure.
- **Specification demo:** Preserve the formal statements while asking an LLM to generate code and proofs. Require a human check of the intended meaning as well as Lean checking. Rehearse the exact prompt and keep a local copy of the checked result for unreliable network access.
- **Bonus scope:** Week 14 implements a constraint-decomposition step, not complete Hindley–Milner inference. Week 15 focuses on observable effect ordering. Week 16 proves two laws rather than trying to build a general category library in one meeting.

## Exam practice

Throughout the exercises, include short prompts of the same form as the offline exam: predict a `#eval` result; identify the type of a fragment; explain an error; state what a theorem claims; identify an induction hypothesis; complete a small definition or proof. The templates supply decreasing amounts of guidance. Week 13's LLM material can appear as a code and proof audit question without any model access.
