---
title: Run declaration participants in a straight line
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.912527+03:00"
---

Problem: src/core/declaration-transaction.f (389 lines) is a general ordered-transaction coordinator (participant table, insertion sort, telemetry, poisoning, seal, xt! callbacks) with exactly one instance (generated-declaration.f:550-608). That instance has six participants with constant orders 100/800/820/830/850/900; its diagnostic is 2drop; its telemetry readers are called only by tests (test/generated-declaration-transaction-suite.f, 1371 lines; test/decl-event-suite.f, 1119 lines). generated-declaration.f:66-68 says declarations never nest, yet four or five participants keep growable per-depth slot arrays (ARM-P generated-declaration.f:687-701, DEV-PART-* decl-event.f:621-648, SM-ARM-* structure-make.f:184-199, BASE-P in generated-declaration-protection.f, FRAME-ROW in generated-declaration-dictionary.f). Acceptance: a straight-line RUN calls the participants in order and rolls back in reverse on catch; one cell per participant slot; the coordinator, its telemetry and its framework-only tests deleted; declaration behavior, including rollback on every failure point, kept and tested through real declarations. About -550 source lines, -1,500 test lines. Design with the Plan agent before dispatch; Fable review. Files: src/core/declaration-transaction.f, src/core/generated-declaration*.f, src/core/decl-event.f, src/core/structure-make.f, test/generated-declaration-transaction-suite.f, test/decl-event-suite.f. Verify: rebuild bin/hb (engine prefix loads declaration-transaction.f); declaration and decl-event suites; bin/hb --load test/run.f. Depends: none. Ownership: listed files. Claim: unassigned.
