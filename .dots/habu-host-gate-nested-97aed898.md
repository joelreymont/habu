---
title: Host-gate nested validation and run-lib tests
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T00:30:43.279865+02:00"
---

Full context: two test files die on non-matching hosts rather than skipping, both discovered by the new bidirectional suite-coverage check. tools/nested-validation-rca-core.f:136 dies with exit 64 off Linux because it reads /proc/self/stat, so test/nested-validation-rca-test.f cannot run on macOS; test/run-lib-test.f exits 66 (missing host profile file) off a DGX Spark host and additionally requires stdin closed. Both currently sit in suite-coverage-lint's documented host-gated table. Apply the printed-skip convention already used by test/gate-env-stdin-tty-test.f so each skips with a recorded reason instead of dying. Note both are self-cleaning: once fixed, their rows in SC-HOST-GATED-TABLE go stale and the lint reds until the rows are removed.
