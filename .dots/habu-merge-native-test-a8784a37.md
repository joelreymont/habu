---
title: Merge native test run into TEST
status: active
priority: 1
issue-type: task
created-at: "2026-07-28T13:29:27.906090+02:00"
---

Claim: agent=codex-test workspace=.jj-ws/codex-test-land

Why: TEST is the sole package owner for test definitions and execution. TEST-RUN is an artificial split with no distinct resource or invariant, and TEST-RUNNER must not replace it. Exact result: every current package TEST-RUN declaration reopens TEST; every live source and documentation package reference uses TEST; no TEST-RUN or TEST-RUNNER package name remains. Preserve the existing public runner operations and behavior. Resolve the only measured TEST namespace collisions without aliases: rename the white-box run-lib test entry RUN to RUN-LIB-TEST, rename its execution site, rename the rerun fixture SETUP to RERUN-SETUP, and keep fixture helpers private. Owner: package TEST. Dependencies: none. Forbidden: compatibility aliases, forwarding words, another test package, behavior changes, phase reorderings, broadened visibility, copied runner logic, or changes to unrelated gate plumbing. Acceptance: rg finds no live `package TEST-RUN`, `using TEST-RUN`, `TEST-RUN:`, or proposed TEST-RUNNER package syntax; unrelated test-word names containing TEST-RUN or TEST-RUNNER stay unchanged. Package and typed-local diff checks pass; test/run-lib-test.f, test/run-rerun-failed-test.f, test/run.f, resident worker paths, lib/test/suite-test.f, and the native test suite preserve exact results. The smallest pre-change structural proof is that test/run.f imports TEST-RUN and the package declarations create a second owner for test execution.

Measured checkpoint: the complete creator-name census across the TEST and TEST-RUN sources found only RUN and SETUP. Baseline focused tests passed, and a representative package rename passed package-diff-lint. The first complete test/run.f execution after the mechanical cutover then exposed one caller cascade: a broad `using TEST` scope crossed the resident require, making global `reset` ambiguous with `TEST:RESET` and a child package's `RUN` ambiguous with `TEST:RUN`. Refrozen correction: test/run.f qualifies only `TEST:PREPARE`, `TEST:EARLY-EXTERNAL-START`, `TEST:DAG-RUN-REST`, and `TEST:COMPLETE`; the JSON performance call stays unchanged. No broad TEST import crosses a child load.

Scope correction: the zero check covers package declarations, imports, qualifiers, and package prose only. Names such as `TEST-RUN-CAPTURE` and `TEST-RUNNER-TEST-MAIN` describe test operations, not packages, and remain unchanged.
