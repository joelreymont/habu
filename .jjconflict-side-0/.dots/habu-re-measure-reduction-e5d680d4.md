---
title: Re-measure reduction timing after WAR fence
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T11:42:27.038830+02:00"
---

The reduction-emitter fence landing (a32d7714) adds one bar.sync per block reduction for architectural correctness. Reduction-heavy kernel timing on the GB10 (softmax, layernorm, rmsnorm rows) should be re-measured on a QUIET box by the single timing lane and the measured rows updated; the Orin rows stay owed with that box. If the fence costs measurable throughput, the emitter may elide the leading barrier on a kernel's FIRST reduction (provably no prior read) - only with the elision proven safe in the emitter test.
