---
title: Rebuild evaluate transaction rollback on master
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T00:42:48.621870+02:00"
---

Verified engine-integrity defect (adjudication 2026-07-19, reproduction re-run by orchestrator on master bin/hb): master's evaluate recovery is pointer-only (EVAL-FRAME restores INP INE RET SP XDS CP NDICT DP plus package snapshot, bootstrap/cg/forth.fs:160+5982) and never undoes content mutations, so a nested evaluate that retires words and then throws tears the dictionary. Proof: after 's" FZ-A" FORGET-DEFS-FROM TRUSTED: FZ-OV ... ; -713 throw' inside a caught evaluate, pre-existing FZ-A is DESTROYED and in-transaction FZ-OV LEAKS live (flags 0/1); undefine inside a failed evaluate likewise persists because the retire flag is a content write no frame restores. HIDE-DEFS-FROM and plain definition growth are covered by pointer restore. Landed DIAG-QUIET/armed-flag hooks (563b2540), sealed PF registry (99aa254d), and the TDECL field-arena transaction cover other domains and do not close this. Fix: composite savepoint keyed to evaluate depth - re-derive eval-retire/eval-forget journaling plus eval-frame content capture in forth.fs/habu2.f, checker usigs snapshot (UEND is a bare variable, checker.f:3050, truncated in place by CHECKER-USIGS-TRUNCATE-FROM-RAW) on top of the 563b2540 typed-defer hooks, and a thin CHECKER-TXN coordinator; land test/eval-xref.f green as the regression. The stranded July-14 engine cannot land mechanically (35-file conflict, depends on campaign-only words); its executable spec is preserved at retired tip 7ce90836 (test/eval-xref.f rebases clean) and its design blueprint at retired tip 13a229b9 - both commits kept un-abandoned as pointers. Seed-affecting when implemented.
