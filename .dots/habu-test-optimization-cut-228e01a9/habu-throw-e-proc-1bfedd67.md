---
title: Throw E-PROC-TIMEOUT for a native-build deadline in chain-run
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-01T12:03:31.881832+02:00\""
closed-at: "2026-10-01T17:16:44.031105+02:00"
close-reason: Fixed by dcadcbe4 (review 166 ACCEPT)
---

Problem: tools/chain-run.f:44-47 BUILD (r4-wblabel f2d76c1c) maps every err rc of native-build to E-BUILD-STATUS, including PROC-TIMEOUT-RC (124), the child's own deadline. The exit contract (124 = PROC-TIMEOUT-RC only; a reader that turns a child status into a verdict throws E-PROC-TIMEOUT; GE-FAIL already does this for test rows) makes that a timeout mislabelled as a build failure. Found by the r4-wblabel c3 worker. Acceptance: a native-build child exiting 124 makes BUILD throw E-PROC-TIMEOUT; any other nonzero rc stays E-BUILD-STATUS with the stderr line; a case forcing the child's exit 124 (a stand-in engine, not a full build) is seen failing first through the real tools/chain-run-test.f load path. The tools' own exit breaks the same contract: lib/process.f's PROC-TIMEOUT-RC comment says a tool whose work can end on a deadline exits PROC-TIMEOUT-RC for E-PROC-TIMEOUT so the verdict reaches the top of a chain, but tools/chain-run-build.f and tools/chain-plan-build.f have no catch boundary, so a deadline ends as the uncaught -2502 and exit 67 (review 140); tools/native-build.f and tools/build-fixpoint.f already map it with PROC-EXIT-RC. Acceptance also: both entries exit 124 on E-PROC-TIMEOUT and their own failure status on any other throw, each through PROC-EXIT-RC as native-build does, shown failing first. Files: tools/chain-run.f, tools/chain-run-test.f, tools/chain-run-build.f, tools/chain-plan-build.f. Verify: tools/chain-run-test.f and tools/chain-plan-test.f rc 0.
