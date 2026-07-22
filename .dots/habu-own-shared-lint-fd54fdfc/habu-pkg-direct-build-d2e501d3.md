---
title: Package direct build lint hooks
status: active
priority: 1
issue-type: task
created-at: "2026-07-22T17:32:17.036300+02:00"
---

Files: tools/hb-build-direct-lints.f and tools/hb-build-direct-lints-test.f only. Add package HB-BUILD-DIRECT-LINTS, keep all state and helper words private, rename HBB-INSTALL-DIRECT-LINTS to INSTALL, and invoke INSTALL inside the package so the existing hook installation side effect is unchanged. Continue calling the still-global signature and AOT lint core APIs in this leaf. The focused test must install distinguishable failing child-hook sentinels before loading the direct adapter, then prove the adapter replaces both sentinels, accepts valid source in process, and propagates each real core's exact failure through HBB-RUN-AOT-LINT and HBB-RUN-SIGNATURE-LINT. It must not call either core directly or merely inspect a hook cell. Acceptance: no HBB-* definition or storage remains global; both direct lint hooks install exactly once and propagate failures; mutations that retain either child hook or drop LINT-EXIT fail; no alias or swallowed error. Verify: the direct-lint focused test, signature-lint-test.f, aot-lint-test.f, typed-local-diff-lint, package-diff focused mutation, host-lint, filemap-lint.

Claim: agent=direct_build_pkg workspace=.jj-ws/habu-pkg-direct-build-d2e501d3.
