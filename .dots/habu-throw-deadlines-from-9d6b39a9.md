---
title: Throw deadlines from test timeout arms
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T08:34:57.368894+03:00"
---

Problem: docs/gate.md:426-440 says a test that MATCHes a capture outcome throws an expired deadline from its timeout arm through T-TIMED-OUT, or SUBJECT:TIMED-OUT when it keeps its own counters, so the pool reports TIMEOUT-UNDER-LOAD instead of a false assertion. These arms answer a value instead: tools/check-verify-core.f:893, test/gate-signal-test.f:393, src/lib/process-tree-test.f:85, test/process-image-subject.f:102, test/gate-pool-orphan-test.f:69, test/check-signal-test.f:302 (timeout OF false); the one-file SUBJECT probes test/c2-memory-scope-refusals.f EXITED?/REPORTED?, test/structure-opaque-program.f STATUS?, test/c2-diagnostics.f:23 REFUSAL and test/c2-init-accessor-refusals.f REFUSED?; and further arms that answer a value: tools/check-verify-test-lib.f:327, :548, :1543, test/cast-suite.f:334, src/lib/policy-test.f:49, test/gate-pool.f:1586, tools/repair-packet-test.f:191, test/host-io-image.f:132. Acceptance: each arm is classified in the commit message as (a) a test arm that turns a deadline into a value: it now throws through T-TIMED-OUT or SUBJECT:TIMED-OUT with the program and capture; (b) a test that expects the deadline: unchanged; or (c) product tool code whose contract reports its own deadline: unchanged, with the reason. Assertions are otherwise unchanged and every touched row passes. Files: the files named above. Verify: the SUITE rows of the touched files. Depends: none. Ownership: those files. Claim: unassigned.
