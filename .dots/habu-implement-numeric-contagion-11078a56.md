---
title: Implement numeric contagion
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:18:00.541491+02:00"
---

Files: src/runtime/primitives/arith.zig
Define contagion helper:
- fixnum → bignum → rational → float → complex
- Operations promote to wider type
- Auto-reduce rationals after ops
- Complex with imag=0 → float
- Bignum in fixnum range → fixnum
Dependencies: habu-implement-bignum-division-769da9e9
Verification: (+ 5 2.0) → 7.0, (/ 4 6) → 2/3
