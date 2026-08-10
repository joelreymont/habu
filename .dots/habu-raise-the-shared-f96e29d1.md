---
title: Raise the shared block ceiling for wide dispatches
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T06:55:29.405677+02:00"
---

A MATCH compiles at most 16 arms: 17 hits E-A64SEL-CAP (select.f R-PUSH queue at NFROZEN:BMAX=64, a ceiling six passes share), 21 hits E-IR-SYM-CAP (module symbol plan), 22 the block ceiling itself. A 44-arm dispatch needs 134 blocks; the tree's largest family (73 variants, the a64 opcode family) needs 221. Measured unlock (match lane 2026-08-10): BMAX 64->224 costs +702KB DP heap in a chain-loading process (1.44->2.14MB) and loads clean. The mismatch-stub removal alternative was probed and rejected: it needs a full compile-time state snapshot (vector, glue, memory order, loop counters, locals, memo) and still does not reach 44 arms. Work: raise BMAX and the module symbol plan together, re-derive any pass that sized buffers off the old value, and move the pinned ceiling rows in test/compiler/native-match.f (RC-WIDE/RC-OVER carry the current numbers - acceptance is moving them to the new ceiling, plus a 44-arm and a 73-variant dispatch executing value-for-value with the engine). Files: src/compiler/native/nfrozen.f (BMAX owner), the six sharing passes, src/compiler/ir/ symbol plan. Depends: none.
