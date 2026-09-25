# Week 05 exercise: Type classes

**Format:** 90-minute exercise session. Work in `Templates/Week05.lean`; compare with `Course/Week05.lean` after attempting the tasks. The bonus task is optional.

## 1. Read and predict (20 minutes)

For `describe TrafficLight.green`, identify the class constraint, the instance, and the returned string.

## 2. Program (45 minutes)

Complete the `Label TrafficLight` instance. Define `Label` for a second type, then call the same generic `describe` function at both types.

## 3. Explain or prove (25 minutes)

Read a missing-instance error produced by calling `describe` on a type with no `Label` instance. Explain what definition Lean is looking for.

## Bonus

Add a `Label` instance for a small tree type.
