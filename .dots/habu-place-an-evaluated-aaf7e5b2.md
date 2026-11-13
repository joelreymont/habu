---
title: "Place an evaluated string's error under tools/check.f"
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T06:54:56.327657+02:00"
---

Problem: run through tools/check.f, test/certify-does-definer.f fails one assert in CDD-SECTION-CLAUSE-TAIL (row 'compiled natively, the clause's unknown type is placed…'): the JSON diagnostic names the file (line 589, column 6) where the row expects "file":"<input>","line":1,"column":4, the place of an evaluated string (Joel 2026-10-31, adding SRCLOC:FILE-BUFFER?: the file alone for an evaluated string). Loaded with --load the row passes. Seen by pkg fold A3's worker: F94 with the fold (jerry-pkg2d), F84 on master d411bc5b's engine and tree (jerry-qmint .out/before/hb), so it predates pkg. Repro: `bin/hb --load tools/check.f -- --all-errors test/certify-does-definer.f; echo rc=$?` (rc 1). Acceptance: reduce it to the smallest program, find whether check.f's scope gives SRCLOC:FILE-BUFFER? or the native compiler's error-place writer the wrong buffer, fix that layer; the command above rc 0. Files: tools/check-core.f, src/core (SRCLOC), the native compiler's error place.
