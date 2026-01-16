---
title: Add bit operations test matrix
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:17:36.793809+02:00"
---

Files: tests/ or arith.zig test section
Add tests (use ohsnap for struct output):
- logbitp: fixnum/bignum × pos/neg × boundary bits (0,31,63,64,127)
- logbitp(-1, N) for N in 0..1000 → all 1
- logbitp with negative index → TypeError
- logbitp with non-integer → TypeError
- logtest: mixed fixnum/bignum types
Dependencies: habu-implement-logbitp-with-acec3ed6, habu-implement-logtest-correctly-c506d6d7
Verification: zig build test passes, all cases correct
