---
title: Resolve qualified and late names in T0 as tier 1 does
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T21:50:37.151512+02:00"
---

Census finding: T0 refuses real words as E-T0-UNSUPPORTED on their name, e.g. TASK:STOP-FOR (src/lib/task.f:2137, row tasking-threads, body STOP-FOR-DRAIN in src/lib/task-test.f), because NDICT:CALL-TARGET answers 0 in t0.f COMPILE-TOKEN and the token falls to NUMBER. Whitebox-only names (CTL-MASKS, TFX-HI, CHECKER-*, TFAM:*) appeared only because the census ran WHITEBOX-SUITE rows on a sealed engine: not a gap. Root-cause the 13 named refusals in /tmp/a64ref out (list in census sec. 3) and fix at the responsible layer.
