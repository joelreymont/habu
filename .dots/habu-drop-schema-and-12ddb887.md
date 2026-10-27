---
title: Drop schema and format versions
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T13:24:03.198957+02:00"
---

Problem: JSON outputs carry schema_version (tools/check-core.f:739, check-all-errors-core.f:267, public-signatures-core.f, repair-packet-core.f, reserved-name-lint-core.f, checked-boundary-lint-core.f, aot-call-report-lib.f, check-verify-core.f, diag-to-sarif-core.f, gate-json-assert-core.f and their tests) and persisted formats carry version constants; Joel 2026-10-27: 'we have no legacy. hard cutover only!'. Acceptance: no schema_version field emitted or asserted; version constants that only gate old data deleted; a constant that keys a cache is replaced only where the key already covers the writer. Files: as listed plus rg -n 'schema_version|VERSION' census. Verify: rows of every touched tool. Depends: none. Ownership: carl. Claim: unassigned.
