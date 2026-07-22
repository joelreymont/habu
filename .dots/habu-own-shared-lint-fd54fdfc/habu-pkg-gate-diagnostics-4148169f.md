---
title: Package gate diagnostics suite
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-22T17:33:12.047682+02:00\""
blocks:
  - habu-pkg-gate-runner-74b02485
  - habu-pkg-diagnostic-worker-3a0f1d49
---

Files: test/gate-diagnostics-lib.f, test/gate-diagnostics-all-strict-lib.f, test/gate-diagnostics-entry-lib.f, test/gate-diagnostics.f, test/run-worker-diag.f, test/run-worker-diag-all-strict.f, and the diagnostic dispatch calls in package GATE-RUNNER. Put every diagnostic-suite region in package GATE-DIAGNOSTICS, make state and helpers private with short tails, publish REPAIR ( -- ), UNDEFINED-PRIMARY ( -- ), ALL-STRICT ( -- ), and FILE-UNSAFE ( -- ) for GATE-RUNNER, and keep command DISPATCH ( -- ) private because test/gate-diagnostics.f executes it inside the package. Continue calling the still-global checker API. Keep generated fixture text unchanged. Acceptance: no GDX-* implementation name remains global; serial and all four worker slices retain exact diagnostics and artifacts; no compatibility alias. Verify: diagnostic gate serial and all four slices, gate runner diagnostic slices, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.

Claim: agent=gate_diagnostics_pkg workspace=.jj-ws/habu-pkg-gate-diagnostics-4148169f.
