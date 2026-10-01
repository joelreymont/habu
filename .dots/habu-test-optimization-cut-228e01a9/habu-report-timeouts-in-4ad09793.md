---
title: Report timeouts in the outcome asserts
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T00:07:43.872591+02:00"
---

Problem: T-OUTCOME-EXITED= and T-OUTCOME-SIGNALED= (lib/test/outcome.f:14,21) throw a bare E-PROC-TIMEOUT when the child times out: no case label, no program and none of the captured output, while T-TIMED-OUT (outcome.f:36) reports the label and SUBJECT:TIMED-OUT's capture. test/compiler/ir-id.f:70 CHILD-EXITED= repeats the same shape. About 75 call sites use them. From review 221 of subjto. Acceptance: these asserts take the program and the out/err spans the caller already holds, and on timeout report through SUBJECT:TIMED-OUT (label, program, output) instead of an anonymous throw; ir-id.f's copy goes, using the shared assert; PROC-OUTCOME>RC stays an rc conversion. A forced-timeout case shows the label and output before and after. Files: lib/test/outcome.f, test/compiler/ir-id.f and every caller. Verify: the callers' suites pass; run the full native suite (shared test library).
