---
title: Remove ENGINE-SNAP-XT-CELL or show its reader
status: closed
priority: 3
issue-type: task
created-at: "\"2026-10-01T04:12:40.715998+02:00\""
closed-at: "2026-10-01T17:19:48.993295+02:00"
close-reason: Fixed by mxvknsxo (review 176 ACCEPT)
---

Problem: with src/habu/snap.f retired (dot 32f3b8fc) ENGINE-SNAP-XT-CELL (src/habu/layout.f:1219-1224, $27F8) has writers (src/habu/habu2.f:2236 bakes CHECKER-CAPTURE-PREPARE into it, :7952 names it; src/habu/snap-lib.f:229 zeroes it) and no production reader: snap.f:54 was the only one. The rest are tests and layout rows (tools/native-layout.f:14, test/top-row-hook-test.f, tools/build-fixpoint-snapshot-test.f, lib/fs-mutate.f:553 comment). Acceptance: either a production reader is shown, or the cell, its baked prefix line, its data-claims and layout rows and the tests that only check it are removed, the engine converges over generations and the layout rows pass. Files: src/habu/layout.f, src/habu/habu2.f, src/habu/snap-lib.f, tools/native-layout.f, the tests named. Verify: test/native-layout.f, test/top-row-hook-test.f, build-fixpoint-snapshot, two-generation build. Depends: habu-load-src-habu-32f3b8fc. Ownership: ENGINE-SNAP-XT-CELL.
