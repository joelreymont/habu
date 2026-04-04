---
title: Partition share tests and modules
status: closed
priority: 2
issue-type: task
created-at: "\"2026-04-01T22:06:02.303346+02:00\""
closed-at: "2026-04-04T17:48:01.416435+02:00"
close-reason: "done: docs/maxima-test-tranches.json now partitions upstream share testsuite order into deterministic owned tranches with expected runtime labels and representative module grouping"
blocks:
  - habu-close-rtest1-infrastructure-f994c408
---

Problem: share execution is too broad to track without explicit tranche boundaries. Acceptance: share tests and representative share modules are partitioned into deterministic tranches. Files: ../maxima/share/**. Verify: share tranche manifest with ownership and runtime. Blockers: habu-close-rtest1-infrastructure-f994c408.
