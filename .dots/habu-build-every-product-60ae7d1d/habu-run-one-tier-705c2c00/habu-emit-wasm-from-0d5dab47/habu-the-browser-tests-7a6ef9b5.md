---
title: The browser tests build their modules with their own hb-build call
status: closed
priority: 3
issue-type: task
created-at: "2026-11-10T20:17:00.865420+02:00"
closed-at: "2026-11-10T21:18:01.817622+02:00"
close-reason: "landed in master 17bf4285; its gate: engines converge, test/wasm/device.f (thirteen test: ok), arm64-link-run.f, error-code-lint.f, diag-position-test.f rc 0, suite 273 of 273, chain fixpoint at generation 2"
---

Problem: test/wasm/harness.f WASM-HARNESS:BUILD ( src mod -- rc ) (:115) runs hb-build --target wasm for a test, and each of test/browser/echo-test.f (:77), display-test.f (:54), navigation-test.f (:53), app-test.f (:115), request-test.f (:107), storage-test.f (:60) and take-test.f (:48) repeats the same argument list. test/wasm/hb-build.f keeps its own call: the command line is its subject. Acceptance: the seven tests build through WASM-HARNESS:BUILD and carry no argument list of their own. Files: the seven test/browser/*-test.f. Verify: each ends test: ok. Depends: none. Ownership: none. Claim: unassigned.
