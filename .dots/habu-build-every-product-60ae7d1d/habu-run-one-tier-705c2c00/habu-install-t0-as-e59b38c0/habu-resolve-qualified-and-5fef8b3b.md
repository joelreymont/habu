---
title: Resolve qualified and late names in T0 as tier 1 does
status: closed
priority: 2
issue-type: task
created-at: "2026-10-28T21:50:37.151512+02:00"
closed-at: "2026-10-28T21:56:51.752915+02:00"
close-reason: "No gap: every refused name was refused by the checker in the same census process (WRAP runs T0 before the checker). TASK:STOP-FOR: engine c2f predates task.f STOP-FOR (156134db); 10 checker internals: WHITEBOX-SUITE rows on a sealed engine, all T0 rc 0 on a whitebox engine; URNW/SECP/TRW-NEVER-DEFINED: negative tests."
---

Census finding: T0 refuses real words as E-T0-UNSUPPORTED on their name, e.g. TASK:STOP-FOR (src/lib/task.f:2137, row tasking-threads, body STOP-FOR-DRAIN in src/lib/task-test.f), because NDICT:CALL-TARGET answers 0 in t0.f COMPILE-TOKEN and the token falls to NUMBER. Whitebox-only names (CTL-MASKS, TFX-HI, CHECKER-*, TFAM:*) appeared only because the census ran WHITEBOX-SUITE rows on a sealed engine: not a gap. Root-cause the 13 named refusals in /tmp/a64ref out (list in census sec. 3) and fix at the responsible layer.
