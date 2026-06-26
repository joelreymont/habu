---
title: Factor bootstrap trust calls
status: closed
priority: 3
issue-type: task
created-at: "\"\\\"\\\\\\\"\\\\\\\\\\\\\\\"2026-06-25T12:19:43.576952+02:00\\\\\\\\\\\\\\\"\\\\\\\"\\\"\""
closed-at: "2026-06-25T14:33:54.419806+02:00"
close-reason: "completed: commit 06652621 factored trust-call helper protocol; bootstrap-codegen-test, trust-lint, stale-status-lint, engine suite, build-helper bundle, full native gate, and local recovery probe passed/recorded"
---

Finding F21. Evidence: docs/factorization-review.md:49; bootstrap/cg/forth.fs:1524. Root cause: trust support mixes lookup/failure policy and raw call argument pushing. Fix: split required trust lookup, argument pushing, and generic save-LR call helper. Why: trust boundaries must be small and auditable. Validate with trust-lint, bootstrap-codegen-test, and full native gate.
