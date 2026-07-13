---
title: Derive inprocess + spawned ptx-toolchain suite lists from one source
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T18:16:23.855603+02:00"
---

Problem: the resident inprocess suite list GSI-LINT-LIBS-PTX-TOOL (test/gate-stdlib-inline-lib.f) and the spawned TEST:SUITE ptx-toolchain (test/gate-stdlib-cases.f) are hand-synced copies that already drifted once (kbench added 7 files to the spawned list only, so test/run.f skipped them). They are now DELIBERATELY different: the spawned list is a superset that includes the device/bench tools (bandwidth-lib-test, fusion-compare, gemm-bench) which SIGBUS when loaded into the resident full-runner image, so the inprocess list carries only the unit tests + perf-regress scan. Fix: derive both lists from one checked source-of-truth that tags each entry inprocess-safe vs spawn-only, OR add a lint that verifies the inprocess list is the spawned list minus the tagged spawn-only entries. Acceptance: a test proving the two lists cannot silently diverge. Files: test/gate-stdlib-inline-lib.f, test/gate-stdlib-cases.f, test/run-lib.f. Verify: the new lint/test + test/run.f. Depends: none. Ownership: gate suite membership. Claim: unassigned.
