# Week 10 exercise: Lambda calculus

**Format:** 90-minute exercise session. Work in `Templates/Week10.lean`; compare with `Course/Week10.lean` after attempting the tasks. The bonus task is optional.

## 1. Read and predict (20 minutes)

Translate `λx.x`, `λx.λy.x`, and `(λx.x) (λy.y)` into De Bruijn terms.

## 2. Program (45 minutes)

Complete `wellScoped`, then complete `evalCBV` using environments and fuel. Test a closed identity, a free variable, and self-application of the identity.

## 3. Explain or prove (25 minutes)

Trace one beta reduction under call-by-value and call-by-name. Explain why substitution needs to avoid variable capture.

## Bonus

Implement a free-variable check for a named-term representation and compare it with `wellScoped`.
