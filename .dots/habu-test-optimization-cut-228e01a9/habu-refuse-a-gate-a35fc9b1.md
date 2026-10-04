---
title: Refuse a gate scratch path past the cap by name
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T18:47:04.790985+03:00"
---

Lane 500 (gatediag, e629c3dd): lib/test/runner.f:94 GT-PATH calls lib/fs.f:455 JOIN-PATH, which throws E-FS-PATH uncaught once the scratch root plus a file name passes FS-PATH-CAP (1024), so any gate row ends on an uncaught throw with an HB_TMP near 1 KB. Acceptance: a row run with an HB_TMP whose joined paths pass the cap fails first with the uncaught throw; the runner refuses such a root once, up front, with a named failure naming HB_TMP and the cap (or the row reports the path refusal as a named test failure); no gate row ends on an uncaught throw for a long scratch root.

Widened (review 522, launchpath): test/gate-pool.f GT-POOL-RETIRE-SLOT -> GT-POOL-CHILD-TMP-REMOVE -> GT-POOL-TREE-REMOVE calls REMOVE-TREE on a slot's HB_TMP from GT-POOL-REAP with no catch, and REMOVE-TREE (lib/fs.f FS-DESCEND-PATH, FS-CHECK-WALK-JOIN-CAP) throws E-FS-CAPACITY -2106 on any tree deeper than FS-PATH-CAP, so one test that leaves such a tree ends the whole pool on an uncaught throw (measured: ~/.cache/tmp/kestrel-r4-rev522/rm.f on the unfixed boot-relocation DEEP tree, -2106). Acceptance adds: a slot whose scratch cannot be removed is reported once as a named failure of that row (path and code), the pool continues, and the row is seen failing first with the uncaught throw.
