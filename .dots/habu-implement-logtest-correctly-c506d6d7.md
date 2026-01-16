---
title: Implement logtest correctly
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:17:29.112293+02:00"
---

Files: src/runtime/primitives/arith.zig
Update logtest:
- Check both args are integers, else TypeError
- Implement: (logtest a b) ≡ (not (zerop (logand a b)))
- Handle fixnum/bignum mixed types (promote)
Dependencies: none
Verification: (logtest 5 3) → t, (logtest 4 3) → nil
