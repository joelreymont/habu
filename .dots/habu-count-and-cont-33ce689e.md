---
title: Count and continue past cast refusals in all-errors mode
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T14:44:08.189924+02:00"
---

Problem (measured 2026-10-02, B10c re-review probes /private/tmp/claude-501/-Users-joel-Work-habu/48f16cef-908f-4f90-a73f-c7636b222de1/scratchpad/rev-b10c/probes2/matrix.txt): under tools/check.f --all-errors any CAST-CERTIFY refusal throws out of CA-RUN-DEFS (tools/check-all-errors-core.f:475-479) and ends the verify pass at that declaration. E-CAST-FAM, E-CAST-OWNER and the other cast rejects print no diagnostic (rc 70), and with --json-errors the run exits 67 'uncaught throw code 7131/7135'. Only the bad-signature cast prints one. A bad ':' definition reports and the pass continues. This predates B10c (lxwpuqzz behaves the same for E-CAST-OWNER). Acceptance: in MULTI-ERR mode every cast refusal renders its diagnostic (prose and JSON, with its code) and counts a reject, and the pass continues to later declarations; a normal load is unchanged. Files: src/core/checker.f CAST-CERTIFY and CHECKER-DEFCAST, src/core/render.f, tools/check-all-errors-core.f, tools/check-test-lib.f. Verify: tools/check-test.f cases where a refused cast is followed by a bad definition and both are reported, in prose and JSON.
