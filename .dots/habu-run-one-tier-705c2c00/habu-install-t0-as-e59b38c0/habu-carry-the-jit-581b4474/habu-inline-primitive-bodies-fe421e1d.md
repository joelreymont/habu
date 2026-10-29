---
title: Inline primitive bodies in T0 as the JIT does
status: open
priority: 1
issue-type: task
created-at: "2026-10-29T09:56:38.279578+02:00"
---

JIT: LCINLINE (src/habu/habu2.f) copies a straight-line engine primitive body in place, so the caller stays a leaf. T0 spills, bl's and saves x30 for one-cell -rot 2dup 2drop 2swap 2over, count, max, min, abs, open-rd, close. The stack words can take T0's existing wide-value path for one-cell values; the rest need front-end rows (t0.f).
