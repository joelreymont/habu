---
title: Lay out a deeply nested sum in linear time
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T14:04:25.862253+01:00"
---

Problem: TYPED-VARIABLE or 1 TYPED-BUFFER of a NEWTYPE over option nested k deep takes 0.25 s at k=512, 0.92 s at 1023, 2.02 s at 1536 and 3.53 s at 2047 (the deep-cut engine; the installed engine alike), while the NEWTYPE and two words calling it take 0.08 s at 2047; the cost is in the cell's layout path (src/core/layout-buffer.f and what it calls), cause unknown. Acceptance: the times per doubling of k stay near 2x from 512 to 2047, with the cause named. Depends: none. Claim: unassigned.
