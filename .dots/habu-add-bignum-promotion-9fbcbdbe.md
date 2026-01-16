---
title: Add bignum promotion to sub
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T10:24:12.521192+02:00"
---

src/runtime/primitives/arith.zig:41 sub function:
1. Add 'if (a.isBignum() or b.isBignum()) return subBignum(heap, a, b);' before fixnum check
2. After overflow check, add fixnum range check (62-bit signed)
3. If overflow or out of range, call subBignum(heap, a, b)
Pattern after mul (line 58-82)
Verification: (- 1 10000000000000000000) returns bignum
