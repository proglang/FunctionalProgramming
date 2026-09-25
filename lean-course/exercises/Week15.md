# Week 15 exercise: Bonus: transformer ordering

**Format:** 90-minute exercise session. Work in `Templates/Week15.lean`; compare with `Course/Week15.lean` after attempting the tasks. The bonus task is optional.

## 1. Read and predict (20 minutes)

Expand `StateT Nat (Except String) A` and `ExceptT String (StateM Nat) A` into functions and result types.

## 2. Program (45 minutes)

Complete `failLose` and `failKeep`: increment the state, then throw an error. Run each from state zero.

## 3. Explain or prove (25 minutes)

Explain why one result contains no final state and the other contains state `1`. Relate the difference to interpreter logging or counters.

## Bonus

Add an environment effect with `ReaderT` and note how to access it.
