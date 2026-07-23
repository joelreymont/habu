---
title: Package stdlib gate worker
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-23T05:06:41.785483+02:00\""
---

Why: the standard-library resident worker is still a package-less executable module, so changing its tail-process dispatch would fail the exact package ownership gate. Required result: test/run-worker-stdlib.f opens package STDLIB-WORKER around the complete current module; every definition, constant, buffer, and cell stays private, a private RUN performs the existing argument validation and dispatch, and RUN is invoked before ;package. Preserve worker identifier routing, arguments, diagnostics, exit codes, and resident-worker behavior byte-for-byte. No public API, alias, copied state, or caller change. Prerequisites: none beyond verified master. Owned result and files: package ownership of test/run-worker-stdlib.f only. Acceptance: no former worker name is globally or externally qualified reachable; the production resident standard-library worker and its current invalid-argument cases remain exact; an ownership mutation fails. Smallest owning-path check: run the resident worker standard-library slice through its actual gate-runner path, plus exact package and typed-local diff checks. Claim: agent=stdlib_worker_pkg workspace=.jj-ws/habu-pkg-stdlib-gate-fb28fe63.
