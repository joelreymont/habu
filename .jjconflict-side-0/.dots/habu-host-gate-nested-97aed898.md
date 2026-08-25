---
title: Host-gate nested validation and run-lib tests
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T00:30:43.279865+02:00"
---

Full context: two test files die on non-matching hosts rather than skipping. tools/nested-validation-rca-core.f:136 dies with exit 64 off Linux because it reads /proc/self/stat, so test/nested-validation-rca-test.f cannot run on macOS; test/run-lib-test.f exits 66 (missing host profile file) off a DGX Spark host and additionally requires stdin closed. Apply the printed-skip convention already used by test/gate-env-stdin-tty-test.f so each skips with a recorded reason instead of dying.
