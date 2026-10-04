---
title: "Make check-test's in-process runs refuse what the CLI refuses"
status: open
priority: 1
issue-type: task
created-at: "2026-10-02T13:19:10.918496+02:00"
---

Problem (fold 311 r4-dupdiag): inside tools/check-test.f (PATH-RUN, REQ-RUN: the in-process check.f harness) a fixture can check rc 0 while bin/hb --load tools/check.f -- <file> refuses the same file E-UNDEFINED rc 70. Fixtures in $HOME/.cache/tmp/kestrel-r4-dupdiag/fx/sh on a4da213d: v1.f (c12 shape) rc 70 both ways; v2.f, v4.f, v6.f, v7.f, req-shadow.f (v1 plus any extra private, public or top-level definition) rc 0 in-process, rc 70 on the CLI; v9.f/v10.f (v1 plus a padding comment) rc 70 both ways. The skip walk marks the same loader byte in both modes, so the difference is after the walk, in the in-process preverify (state left from earlier cases or the harness's own load). Any in-process acceptance may be a false pass. Acceptance: reduce the cause; the in-process run gives the CLI's verdict on every fixture above (a case asserting both agree, seen failing first); the cases in tools/check-test-lib.f that relied on the false pass are corrected. Files: tools/check-test-lib.f, tools/check-core.f.

Second reproduction (lane 308 r4-doesname): checking the same source twice in one process goes wrong when the source renders a package's words (CKR-MAKE inside package CKR-A) and the pre-pass refuses it: the second check reports E-UNDEFINED CKR-A:CKR-SEVEN instead of leaving it to the run. $HOME/.cache/tmp/kestrel-r4-doesname/probe2.f reproduces it with a colon refusal and no does> on the 4f2a202a engine ($HOME/.cache/tmp/kestrel-r4-doesname/bak/hb.orig). Suspected: checker-scope rollback or check-core state between in-process runs.
