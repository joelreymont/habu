---
title: "Implement logbitp with two's-complement"
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:17:23.635795+02:00"
---

Files: src/runtime/primitives/arith.zig
Update logbitp:
- Check index >= 0, else TypeError
- Check integer arg type, else TypeError
- For negative fixnum: (value >> index) & 1 (sign-extended)
- For negative bignum: two's-complement bit lookup
- For positive: current logic
Verification: logbitp(-1, 0..1000) all return 1
