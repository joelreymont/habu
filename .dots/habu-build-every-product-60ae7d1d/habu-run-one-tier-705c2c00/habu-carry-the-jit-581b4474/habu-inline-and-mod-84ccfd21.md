---
title: Inline / and mod in T0 as the JIT does
status: closed
priority: 1
issue-type: task
created-at: "2026-10-29T09:56:38.289352+02:00"
closed-at: "2026-11-04T11:48:47.832936+02:00"
close-reason: "Census 2026-11-04, done: Landed 35e8b7c8 \"Divide by a literal inline in T0\" plus 50ca4b76; jerry-t0d1 not listed."
---

JIT emits sdiv, or sdiv+msub, inline; T0 calls and opens a frame. zz LEAP-YEAR? calls mod 3 times: +33.8 instructions per call measured, about +90M on the zz run (call count inferred). Needs a seam division operation (t0-emit.f DIVISION says the seam has none, so a zero divisor must throw, not reach sdiv) and constant folding of / and mod.

mrrxlutr + fold pwxnuopw (jerry-t0d1): any constant divisor except 0 and -1 divides in place; reviewed, re-review fold-ins applied (inline divisor row, header).
