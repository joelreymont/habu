---
title: Package direct build lint hooks
status: active
priority: 1
issue-type: task
created-at: "2026-07-22T17:32:17.036300+02:00"
---

Files: tools/hb-build-direct-lints.f, tools/hb-build-direct-lints-test.f, FILEMAP.md, test/run-worker-aot.f, test/gate-stdlib-cases.f, and tools/suite-coverage-lint-core.f only. Add package HB-BUILD-DIRECT-LINTS, keep all state and helper words private, rename HBB-INSTALL-DIRECT-LINTS to INSTALL, and invoke INSTALL inside the package so the existing hook installation side effect is unchanged. Continue calling the still-global signature and AOT lint core APIs in this leaf.

The focused test must install distinguishable failing child-hook sentinels before loading the direct adapter. Before the valid-source assertions, point the public CLI tool override at a proved-missing executable below the test's private temporary root. The direct AOT and signature calls must still succeed in process; a retained child hook must fail because it consults that missing executable. Clear the override before returning so the resident AOT worker retains its original environment. The test must then propagate each real core's exact failure through HBB-RUN-AOT-LINT and HBB-RUN-SIGNATURE-LINT in fresh child processes. It must not call either core directly or inspect a hook cell.

Enroll the focused test in the hb-build-fixtures suite and load it before tools/hb-build-direct-lints.f in the native AOT-positive worker, so test/run.f executes the proof on the production candidate and the test itself installs the adapter for the remaining worker. Extend the derived suite-coverage scheduler scan to include test/run-worker-aot.f; do not classify this scheduled proof as a manual exception.

Acceptance: no HBB-* definition or storage remains global; both direct lint hooks install exactly once and propagate failures; mutations that retain either child hook, omit either direct installation, or drop LINT-EXIT fail; the native AOT-positive phase executes the focused proof; suite coverage derives that enrollment; FILEMAP names the proof; no alias, manual-suite exception, or swallowed error. Verify: the direct-lint focused test; native AOT-positive phase through test/run.f; hb-build-fixtures selection; signature-lint-test.f; aot-lint-test.f; suite-coverage-lint; typed-local-diff-lint; package-diff focused mutation; host-lint; filemap-lint.

Claim: agent=direct_build_pkg workspace=.jj-ws/habu-pkg-direct-build-d2e501d3.
