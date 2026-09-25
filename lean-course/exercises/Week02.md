# Week 02 exercise: Datatypes and recursion

**Format:** 90-minute exercise session. Work in `Templates/Week02.lean`; compare with `Course/Week02.lean` after attempting the tasks. The bonus task is optional.

## 1. Read and predict (20 minutes)

List the constructors of `Suit`. Explain why `Card` is a product of a suit and rank while `Suit` is a choice among alternatives.

## 2. Program (45 minutes)

Complete `isRed` and `countRed`. Add `countSuit (wanted : Suit) : List Card → Nat`, covering both list constructors.

## 3. Explain or prove (25 minutes)

Explain why `countRed` terminates. Trace its result on a three-card list and identify the result of each recursive call.

## Bonus

Extend `Rank` with a joker and revise one function that consumes it.
