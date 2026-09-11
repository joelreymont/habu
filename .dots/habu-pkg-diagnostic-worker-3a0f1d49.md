---
title: Package diagnostic worker entries
status: closed
priority: 1
issue-type: task
created-at: "2026-07-23T00:58:14.595903+02:00"
closed-at: "2026-07-23T02:05:41.109768+02:00"
close-reason: Landed and remotely verified at ad47ef725011; independent destruction review and exact owning/master gates green.
---

Files: test/run-worker-diag.f and test/run-worker-diag-all-strict.f only. The existing TRWD-* definitions fail the package ownership gate because the executable wrappers have no namespace owner. Put both wrappers in package TEST-RUN. In each file keep the entry and all state/helpers private, capture the private DIAG or DIAG-ALL execution token while TEST-RUN is open, close the definition scope, then execute the carried token at top level. This makes package lifetime structural without exporting an entry or storing it in a global cell. Continue calling the current global diagnostic API in this prerequisite; the diagnostic package leaf will qualify those private calls after it lands.

Preserve TRW-LOAD-DONE ordering, resident-id dispatch, typed idx conversion, child labels, fork order, pool drain, and all four slices exactly. Open-package execution is not a behavioral failure on the current diagnostic path and must not be claimed as one. The owning pre-change failure is the measured package-diff rejection of the global TRWD-* definitions.

Acceptance: no TRWD-* definition or storage remains global; TEST-RUN is closed before either carried entry executes; neither private entry resolves globally or as TEST-RUN:DIAG/DIAG-ALL; direct and pooled diagnostic workers retain exact diagnostics, artifacts, statistics, and exits; removing package ownership fails the package gate; no alias, public surface, global execution-token cell, or package exception. Verify: all four resident diagnostic slices, pooled all-strict worker, gate-runner diagnostic slices, private-resolution rejection, typed-local-diff-lint, package-diff ownership mutation, host-lint, filemap-lint.

Claim: agent=diagnostic_worker_pkg workspace=.jj-ws/habu-pkg-diagnostic-worker-3a0f1d49.
