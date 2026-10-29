---
title: T0 joins for do loops, case and MATCH
status: open
priority: 1
issue-type: task
created-at: "2026-10-29T05:11:01.114852+02:00"
---

Lane 3b of the joins design. do/?do/loop/+loop/leave arrive in the head layout; LOOP-NEXT/STEP set the condition and LOOP-BACK branches; of/endof/endcase and ;MATCH reconcile to the case layout; HOLD removed. Tests LOOP-JOIN-CASE.

Joel 2026-10-29: T0 takes the JIT's techniques. Start from the JIT's do loop: the innermost index cached in x29 (src/habu/regalloc.f LIDX, habu2.f LCEMITBL clears it at calls), `i` one mov, values kept in registers across the loop. Census (T0 at mxsxwkkn): T0 spills every value at do/loop/+loop and keeps the index in memory; `i` is 3 loads, `loop` 4 loads and a store (JIT 2 loads, 1 store). zz has 32 ?do/loop and 47 i.
