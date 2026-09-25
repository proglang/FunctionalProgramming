# Week 07 exercise: Parsers

**Format:** 90-minute exercise session. Work in `Templates/Week07.lean`; compare with `Course/Week07.lean` after attempting the tasks. The bonus task is optional.

## 1. Read and predict (20 minutes)

For `Parser α`, identify where the parsed value, remaining input and failure are represented.

## 2. Program (45 minutes)

Complete `character`, `mapParser` and `sequence`. Build a parser for two exact characters and run it on both matching and mismatching input.

## 3. Explain or prove (25 minutes)

Explain why `mapParser` must keep the remaining input unchanged. Explain why `sequence` passes the first parser’s remainder to the second.

## Bonus

Add a parser that accepts one of two chosen characters.
