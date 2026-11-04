---
title: T0 fused compare and branch
status: closed
priority: 1
issue-type: task
created-at: "2026-10-29T05:11:01.098893+02:00"
closed-at: "2026-11-04T11:48:47.795648+02:00"
close-reason: "Census 2026-11-04, done: Landed a2e55d06 \"Fuse T0 compares into their branches\" plus d2a8cf22."
---

Lane 2 of the joins design, after lane 1. One-token peek after a compare: if/while/until consume the condition codes (PENDING); seam IF-CC, WHILE-CC, UNTIL-CC, CC-NOT, condition-preservation rule; x64 DSP+ via lea and LIT-R zero via mov. Tests FUSE-CASE, a row decoding cmp+b.cond, an x86 seam row.
