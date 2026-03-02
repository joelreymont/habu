---
title: Partition share tests and modules
status: open
priority: 2
issue-type: task
created-at: "2026-04-01T22:06:02.303346+02:00"
blocks:
  - habu-close-rtest1-infrastructure-f994c408
---

Problem: share execution is too broad to track without explicit tranche boundaries. Acceptance: share tests and representative share modules are partitioned into deterministic tranches. Files: ../maxima/share/**. Verify: share tranche manifest with ownership and runtime. Blockers: habu-close-rtest1-infrastructure-f994c408.
