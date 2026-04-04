---
title: Partition core tests into tranches
status: closed
priority: 2
issue-type: task
created-at: "\"2026-04-01T22:06:02.278834+02:00\""
closed-at: "2026-04-04T17:48:01.374270+02:00"
close-reason: "done: docs/maxima-test-tranches.json now partitions upstream core testsuite order into deterministic owned tranches with expected runtime labels"
blocks:
  - habu-close-rtest1-infrastructure-f994c408
---

Problem: the core suite is too large to sweep without an explicit tranche plan. Acceptance: core tests are partitioned into deterministic tranches with ownership and expected runtime. Files: ../maxima/tests/**, manifest runner inputs. Verify: tranche manifest checked into docs or tooling. Blockers: habu-close-rtest1-infrastructure-f994c408.
