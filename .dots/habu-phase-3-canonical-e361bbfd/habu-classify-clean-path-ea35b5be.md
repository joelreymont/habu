---
title: Classify clean-path rtest1 failures
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.266993+02:00"
blocks:
  - habu-make-test-batch-57fe1a9d
---

Problem: remaining rtest1 failures are not yet reduced to concrete Habu buckets on the clean path. Acceptance: every remaining rtest1 failure is classified as reader, package, compiler, conditions, streams/pathnames, numeric tower, CLOS/MOP, or GC/JIT. Files: ../maxima/tests/rtest1.mac and failing Habu subsystems. Verify: machine-readable failure inventory with no vague bucket. Blockers: habu-make-test-batch-57fe1a9d.
