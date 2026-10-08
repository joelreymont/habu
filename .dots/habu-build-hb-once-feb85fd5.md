---
title: "Build hb once as the test suite's prelude"
status: open
priority: 2
issue-type: task
created-at: "2026-10-08T10:58:28.842931+02:00"
---

Problem: building is repeated far more than the work needs. The per-landing check carl ran (scratchpad gate.sh) built the engine four ways: the snapshot build, a two-generation build, Gforth recovery and the census (tools/build-fixpoint.f refresh: glued-text certify plus up to four stage generations), against CLAUDE.md, which already makes multi-generation and Gforth occasional. Inside the suite at least 14 test files run an engine build or the build tooling themselves: test/aot-chain-producer-suite.f, baked-owner.f (requires tools/build-fixpoint.f), certify-generated.f, cold-runtime-test.f, compiler/native-hookless-reject.f, gate-aot-positive-lib.f, gate-build-hbb.f, host-checker-row-e2e.f, native-build-entry.f, native-source-bootstrap-child.f, native-source-view-child.f, pre-trust-defer.f, source-root-exe-test.f, whitebox-key.f.
Ruling (Joel, 2026-10-08): build hb once as the prelude of the test suite and run every test against it. The census is deleted. The two-generation build and the Gforth bootstrap (from zero) are occasional integration tests; the snapshot build makes the product.
Acceptance: the suite builds hb once and every test uses it; tests whose subject is the builder move to an occasional integration set, the rest use the prelude's hb; docs/gate.md states the per-landing check (build once + suite) and when the integration set runs.
Files: test/run.f, test/gate-stdlib-cases.f, the 14 tests, docs/gate.md.
Verify: full suite wall time and its count of engine builds before and after.
Depends: none. Ownership: unassigned. Claim: unassigned.
