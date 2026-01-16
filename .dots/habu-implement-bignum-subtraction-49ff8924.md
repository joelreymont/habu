---
title: Implement bignum subtraction
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:17:48.604120+02:00"
---

Files: src/runtime/primitives/arith.zig
Implement multi-limb subtraction:
- All sign combos: pos-pos, pos-neg, neg-pos, neg-neg
- Borrowing for multi-limb
- Normalize result (remove leading zeros, check fixnum range)
Use GMP bindings or implement directly.
Dependencies: habu-audit-numeric-tower-e7a1c39f
Verification: (- bignum bignum) works correctly
