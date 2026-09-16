---
title: colon at end of input is accepted silently
status: closed
priority: 1
issue-type: task
created-at: "\"2026-08-22T22:47:07.208879+02:00\""
closed-at: "2026-09-16T14:34:48.723849+03:00"
close-reason: "superseded by habu-campaign-c4-diagnostics-3b6de147: The reader half landed but nothing audits PEND-CELL at exit, so an evaluate ending inside a definition still returns cleanly and leaks the open record"
---

Problem: src/habu/habu2.f:6115 EM-INTERPRET-COLON calls LTOK with no x0 check (C-TRUSTED 3236, C-CAST 3353, C-DEFER 3560, C-PACKAGE 6318 all check); habu1.f:3136 TOK-NONE leaves TKA/TKL at the previous token (':' itself) so a pending record named ':' is written and PEND-CELL set; EM-COMPILE-EXIT (8389-8409) and EM-EVAL-CLEAN-EXIT (8330-8345) never audit PEND-CELL. A truncated --load file or piped program ending inside a definition exits 0; 's" : FOO 1 2" evaluate' returns cleanly and the caller's next top-level tokens compile into FOO (EM-COMMENT 6092 dispatches on PEND-CELL). Mirror identical (forth.fs:4679, 6420-6427, 6441-6450). Acceptance: Checker-Miss RCA; EM-INTERPRET-COLON dies named on x0=0 through LCOMPILEDIE; LEXIT/EM-EVAL-CLEAN-EXIT refuse PEND-CELL<>0 with a named code (catchable inside evaluate, fail-closed at top level); tests for a truncated file and for the evaluate leak; mirror follows. Files: src/habu/habu2.f, habu1.f, bootstrap/cg/forth.fs, test/. Verify: the tests under bin/hb; recovery gate. Depends: none. Ownership: reader. Claim: unassigned.
