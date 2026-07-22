---
title: Package direct build lint hooks
status: active
priority: 1
issue-type: task
created-at: "2026-07-22T17:32:17.036300+02:00"
---

Files: tools/hb-build-direct-lints.f, tools/hb-build-direct-lints-test.f, FILEMAP.md, test/gate-stdlib-cases.f, test/candidate-validation.f, test/candidate-validation-test.f, and test/gate-validation-worker.f only. Add package HB-BUILD-DIRECT-LINTS, keep all state and helper words private, rename HBB-INSTALL-DIRECT-LINTS to INSTALL, and invoke INSTALL inside the package so the existing hook installation side effect is unchanged. Continue calling the still-global signature and AOT lint core APIs in this leaf.

The focused test must install distinguishable failing child-hook sentinels before loading the direct adapter. Before the valid-source assertions, point the public CLI tool override at a proved-missing executable below the test's private temporary root. The direct AOT and signature calls must still succeed in process; a retained child hook must fail because it consults that missing executable. Clear the override before returning so the resident AOT worker retains its original environment. The test must then propagate each real core's exact failure through HBB-RUN-AOT-LINT and HBB-RUN-SIGNATURE-LINT in fresh child processes. It must not call either core directly or inspect a hook cell.

Enroll the focused test in the hb-build-fixtures suite and as one positive shared candidate-validation case. The shared case must run once on the exact candidate and once on the baseline engine, compare the existing digest-exact evidence, and keep the declared positive tally exact. Extend the candidate-validation whitebox path inventory to count both test/ and tools/ case rows without loosening its one-path/one-kind proof. This uses the existing package-owned candidate-validation interface that suite coverage already derives; do not edit the legacy suite-coverage controller or classify the proof as a manual exception.

The focused proof necessarily spawns two real failure children in each shared worker. Candidate plus baseline therefore raise the measured candidate-validation process-exec count by exactly four, from 8 to 12. Raise the package-owned nested-execution ceiling to 12, document that breakdown beside the constant, and pin the new ceiling in the existing candidate-validation whitebox. A higher allowance, a missing measurement, or any count above 12 remains a failure.

Acceptance: no HBB-* definition or storage remains global; both direct lint hooks install exactly once and propagate failures; mutations that retain either child hook, omit either direct installation, or drop LINT-EXIT fail; the shared candidate-validation phase executes and compares the focused proof with measured nested-exec=12 and rejects 13; suite coverage derives that enrollment from candidate-validation.f; FILEMAP names the proof; no alias, manual-suite exception, or swallowed error. Verify: the direct-lint focused test; shared candidate-validation phase through test/run.f; candidate-validation whitebox test; hb-build-fixtures selection; signature-lint-test.f; aot-lint-test.f; suite-coverage-lint; typed-local-diff-lint; package-diff focused mutation; host-lint; filemap-lint.

Claim: agent=direct_build_pkg workspace=.jj-ws/habu-pkg-direct-build-d2e501d3.
