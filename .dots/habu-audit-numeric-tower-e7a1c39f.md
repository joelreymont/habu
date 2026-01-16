---
title: Audit numeric tower implementation
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:17:42.813034+02:00"
---

Files: src/runtime/primitives/arith.zig, src/runtime/objects.zig
Check which operations support all types:
- +, -, *, / for fixnum/bignum/rational/float/complex
- Verify Rational.reduce() exists and is called
- Verify Complex operations exist
- List missing implementations.
Verification: audit complete, gaps identified
