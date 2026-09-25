# Functional Programming in Lean: authoring draft

This is a new course package for one 90-minute lecture and one 90-minute exercise session each week. Weeks 1–13 form the core. Weeks 14–16 are independent bonus lectures. It is designed for students who know Python and C++ and have studied logic, but have not used a functional language.

## Files

- `slides/WeekXX.md` is the editable Beamer slide source. `slides/WeekXX.pdf` is the rendered deck.
- `Course/WeekXX.lean` is the completed, compiling example used in the lecture.
- `Templates/WeekXX.lean` contains deliberate `sorry` placeholders for live completion. Templates are not imported into the course library.
- `exercises/WeekXX.md` suggests a 90-minute session with a short optional extension.
- `INSTRUCTOR_GUIDE.md` gives timing, the dependency sequence, and old-to-new source mapping.
- `SPEC_DEMO.md` gives the week 13 LLM demonstration script and audit procedure.

## Build and verify

The project pins Lean `v4.33.1` in `lean-toolchain` and uses Lean's bundled `Std` library. From this directory:

```sh
lake build
./validate_templates.sh
./build_slides.sh
```

`lake build` checks the completed examples. Template checks tolerate warnings about the intentional `sorry` placeholders, but should have no errors. Slide rendering requires Pandoc and XeLaTeX. The PDFs are included for classroom use; edit the Markdown files and rerun the build script to revise them.

The completed code is the reference version for live coding. The slide decks are deliberately concise because the lecture plan reserves substantial time for editing and discussing the template in Lean. Use the instructor guide alongside the slides.

## Reading

- [Functional Programming in Lean](https://lean-lang.org/functional_programming_in_lean/) is the main companion text. Check that its examples still match the pinned toolchain before assigning exact pages in a later offering.
- [Lean Language Reference](https://lean-lang.org/doc/reference/latest/) is a reference for definitions, tactics and macros.
- The original Haskell material remains in the repository's `slides/` and `code2024/` directories. The mapping is in `INSTRUCTOR_GUIDE.md`.

## Assessment alignment

The core lectures build toward reading Lean code and proof fragments, explaining type errors, and completing short programs. The exam is assumed to be computer-based, offline, and closed book. The week 13 LLM demonstration is about the relation between a formal statement, an implementation, and a checked proof; it does not presume LLM access in the exam. Bonus lectures can be offered without changing core assessment coverage.
