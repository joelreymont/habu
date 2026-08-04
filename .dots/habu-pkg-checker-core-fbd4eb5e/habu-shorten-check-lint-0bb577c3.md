---
title: Shorten CHECK lint phases
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T02:13:31.672248+02:00"
blocks:
  - habu-shorten-check-child-b122b45d
---

Why: strict signatures, checked boundaries, reserved names, and all-errors are distinct live providers, but their CHECK-private phase adapters still expose legacy CHK names and mutable bridge state. Owner: package CHECK. Files: tools/check-core.f and tools/check-test-lib.f. Rename only the private strict, checked-boundary, reserved-name, and all-errors adapters and storage from RUN-STRICT through RUN-ALL, including all-errors per-file action/result/support/list dispatch and flush. Use short package-local tails; call each provider only through its declared package API; keep bridge state private and reset on every path. Preserve phase order, original-file dependency order, support accumulation, duplicate collection, fatal throw propagation, source/list distinctions, strictness options, diagnostic routing, and result codes exactly. Acceptance: zero executable CHK-prefixed name remains in this concern; comments, strings, wrong hook roles, stale hooks, reserved names, checked boundaries, duplicate definitions across ordered files, provider throws, and JSON/prose modes exercise real CHECK provider calls with byte-exact diagnostics. Forbidden: aliases, copied validators, provider state access, error masking, second support graph, synthetic fixtures that bypass CHECK, or changed phase order. Pre-change proof: a representative short phase adapter fails package ownership outside CHECK and passes only as CHECK-private. Verify through tools/check-test.f strict/boundary/reserved/all-errors slices, the current checked-boundary and provider fixtures, exact diff ownership/type, and gate diagnostics.
