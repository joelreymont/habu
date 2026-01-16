---
title: Implement bignum division
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:17:54.304684+02:00"
---

Files: src/runtime/primitives/arith.zig
Implement multi-limb division:
- Knuth Algorithm D or GMP binding
- Return quotient and remainder
- Sign handling per CL spec
- Division by zero → DivisionByZero error
- / operator: return rational if remainder ≠ 0
Dependencies: habu-implement-bignum-subtraction-49ff8924
Verification: (/ 10 3) → 10/3, (/ 10 2) → 5
