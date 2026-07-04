---
title: event-closure-test flaky E-STR-CAPACITY in gate fork
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T23:33:07.384781+02:00"
---

tools/event-closure-test.f failed once under the full gate (stdlib/tail-pure parallel group) with 'fork worker throw rc -2201' (E-STR-CAPACITY, lib/errors.f:28) on tree 77f2e0bd (item-7 rebase onto 69e152562090). Standalone run of the same test on the same tree passes; immediate gate rerun passes. Not deterministic; not path-length (both gate roots were 15-digit hb-gate ids). Suspect a parallel-group race or a borderline string-builder capacity in the fork-worker capture path. Evidence log (first red run): /var/folders/98/l2ptpkyn41q7d3sp6x4xp87m0000gn/T//hb-gate-887218509776916-7/pool-0-12-23-err.log. Next: reproduce under repeated parallel gate runs, instrument the throw site (WHY-THREW-style catch reporting the throwing word), fix capacity/race at root cause.
