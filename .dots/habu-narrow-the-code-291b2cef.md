---
title: Narrow the code-region protection toggles
status: open
priority: 2
issue-type: task
created-at: "2026-08-11T17:49:45.573381+02:00"
---

Measured by the stageb-design lane (2026-08-11), pays on every boot the gate already runs: the engine flips mprotect over the WHOLE 8MB REGION twice per definition (8.61us/call at 8MB vs 0.41us at 64KB, C microbench; 119ms of the chain load measured, ~10.6% of load cost and ~10% of every boot), and the (PROT-SPAN) store-range guard costs another ~9.2%. The LPROTREC narrow-flip precedent already exists at habu2.f:2179 - apply the same page-range narrowing to the per-definition code-region toggle, and measure whether the (PROT-SPAN) guard can cheapen without weakening (it is a guard - do not delete it; make it cost what it needs to cost). Acceptance: boot wall time drops measurably (baseline 0.40s, mprotect share ~40ms + guard share), full gate green, fixpoint byte-stable x2, the protection INVARIANT unchanged (a store outside the open span still refuses - keep the negative test). Files: src/habu/habu2.f. Depends: none.
