---
title: Batch engine fixture source-list checks
status: closed
priority: 1
issue-type: task
created-at: "2026-06-29T01:57:08.582916+02:00"
closed-at: "2026-06-29T02:02:15.888554+02:00"
close-reason: "completed: replaced four duplicate engine fixture source-list checker subprocesses with one combined dependency-ordered support source-list in test/gate-engine-lib.f; focused fixtures fell from 32.36s to 21.98s wall; full hot gate passed at 45.631s internal / 48.62s wall with helper-spawn=110"
---

Problem: test/gate-engine-lib.f fixture slice runs four separate GE-CHECK-SRC-LIST subprocesses for fs-mutate, process-argv, process-env, and process-cwd support libs; focused fixtures spend about 24s on those checks while the run fixtures take about 7.6s. Fix: replace the four duplicate checker-warm source-list launches with one combined unique support source-list that covers all same libs in dependency order. Files: test/gate-engine-lib.f, STATUS.md, LESSONS.md, gate dots. Acceptance: focused engine fixtures shrink, full hot gate passes, and boundary run fixtures remain separate.
