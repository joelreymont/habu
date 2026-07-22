---
title: Package diagnostic worker entries
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-23T00:58:14.595903+02:00\""
---

Files: test/run-worker-diag.f and test/run-worker-diag-all-strict.f only. Put both executable wrappers in package TEST-RUN. In each file keep the entry and all state/helpers private, capture the private DIAG or DIAG-ALL execution token while TEST-RUN is open, close the package, then execute the carried token so generated package fixtures and child work start from top-level scope. Leave no public API or global entry storage. Continue calling the current global diagnostic API in this prerequisite; the diagnostic package leaf will qualify those private calls after it lands. Preserve TRW-LOAD-DONE ordering, resident-id dispatch, typed idx conversion, child labels, fork order, pool drain, and all four slices exactly. Acceptance: no TRWD-* definition or storage remains global; TEST-RUN is closed before either entry executes; direct and pooled diagnostic workers retain exact diagnostics, artifacts, and exits; no alias or package exception. Verify: all four resident diagnostic slices, pooled all-strict worker, gate-runner diagnostic slices, a negative fixture proving execution inside the open package fails with the nested-package diagnostic, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.

Claim: agent=diagnostic_worker_pkg workspace=.jj-ws/habu-pkg-diagnostic-worker-3a0f1d49.
