---
title: "Infer M0: adapter execution protocol"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:00:35.686571+02:00"
blocks:
  - habu-infer-m0-schema-27e8ca5d
---

Why this exists:
Every baseline adapter needs the same request, process-lifecycle, timeout, log-capture, and result contract. Without that contract, each adapter can hide failures differently.

Required result:
Define one engine-adapter protocol that accepts a workload cell and returns either a complete raw run record or a named failure. It owns start, readiness, request, timeout, shutdown, exit status, and raw-log identity without containing engine-specific flags.

Done when:
Fixture adapters prove successful execution, readiness failure, malformed output, timeout, nonzero exit, shutdown failure, and cleanup after every outcome. No failure is converted into a benchmark sample.

Expected touch points: the shared adapter protocol under tools/infer-bench/ and focused tests.
Smallest check: the focused adapter lifecycle test.
Prerequisites: benchmark record schema.
Owned result: shared adapter request, lifecycle, and raw-result contract only.
Claim: unassigned.
