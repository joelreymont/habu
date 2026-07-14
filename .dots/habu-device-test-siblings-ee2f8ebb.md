---
title: Device-test siblings need compile-check coverage
status: active
priority: 3
issue-type: task
created-at: "\"2026-07-14T19:41:51.403228+02:00\""
---

Found by the devproof lane 2026-07-14: tools/ptx/acc-device-test.f, redadd-device-test.f, saxpy-v4-tail-device-test.f, sum-launch.f, softmax-launch.f, softmax-gradcheck.f run their device leg unconditionally at load (no CUDA:OPEN? recorded SKIP), so they are Orin-only manual tools wired into no suite - a codegen change that breaks their load is not caught off-device by any automatic gate. Fix: add the CUDA:OPEN? device-SKIP guard (fusion-compare/gemm-bench/attention-bench pattern) to each, then wire them as SPAWN-ONLY compile-checks in the ptx-toolchain spawned suite + the suite-coverage-lint SPAWN-ONLY table (conscious-decision entry). cuda-launch.f already gained its guard and compile-checks off-device - wire it first. Acceptance: each file check-loads off-device with a recorded SKIP; suite-coverage-lint 0 findings with the new entries; on-device behavior unchanged (zed-device-suite still drives the launchers). Files: the six tools/ptx files, test/gate-stdlib-cases.f, tools/suite-coverage-lint-core.f, FILEMAP.md untouched. Verify: lint-libs slice, suite-coverage-lint, one on-device zed-device-suite pass. Ownership: ptx device tools.

Claim: agent=devsibs workspace=.jj-ws/fable-devsibs
