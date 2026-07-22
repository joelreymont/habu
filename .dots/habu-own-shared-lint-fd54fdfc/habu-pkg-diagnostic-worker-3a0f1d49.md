---
title: Package diagnostic worker entries
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-23T00:58:14.595903+02:00\""
---

Files: test/run-worker-diag.f and test/run-worker-diag-all-strict.f only. Put both executable wrappers in package TEST-RUN. In run-worker-diag.f keep DIAG private and call it before closing the package. In run-worker-diag-all-strict.f keep the fork id, all helpers, and DIAG-ALL private and call DIAG-ALL before closing the package. Continue calling the current global diagnostic API in this prerequisite; the diagnostic package leaf will qualify those private calls after it lands. Preserve TRW-LOAD-DONE ordering, resident-id dispatch, typed idx conversion, child labels, fork order, pool drain, and all four slices exactly. Acceptance: no TRWD-* definition or storage remains global; direct and pooled diagnostic workers retain exact diagnostics, artifacts, and exits; no alias or package exception. Verify: all four resident diagnostic slices, pooled all-strict worker, gate-runner diagnostic slices, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.

Claim: agent=diagnostic_worker_pkg workspace=.jj-ws/habu-pkg-diagnostic-worker-3a0f1d49.
