---
title: Inline / and mod in T0 as the JIT does
status: open
priority: 1
issue-type: task
created-at: "2026-10-29T09:56:38.289352+02:00"
---

JIT emits sdiv, or sdiv+msub, inline; T0 calls and opens a frame. zz LEAP-YEAR? calls mod 3 times: +33.8 instructions per call measured, about +90M on the zz run (call count inferred). Needs a seam division operation (t0-emit.f DIVISION says the seam has none, so a zero divisor must throw, not reach sdiv) and constant folding of / and mod.
