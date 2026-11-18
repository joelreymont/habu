---
title: "Delete the dictionary writers' run-time guards"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T13:55:44.382196+01:00"
---

Problem: ndict!, cp!, retire-from and record-facts! have only private rows (src/habu/prims.f), so no program passes them a value; only the engine packages that own the rows do. Yet the kernel still checks every call at run time: ndict!'s count bounds (E-DICT-COUNT above DICT-CAP, E-SEAL-VIOLATION below the sealed count, src/habu/habu1.f BNDSET), its raise path that rebuilds the lookup index, record-facts!'s index checks (habu1.f BRECFACTS), cp!'s code-slot bounds (BCPSET), and their x86-64 twins in src/habu/kernel-x64.f (carl's). Acceptance: each such check is deleted, or the engine caller it protects is named as path:line with the input that reaches it; the raise path is deleted unless an engine caller raises the count (src/habu/interpreter.f FRAME-ROLLBACK-CODE, and the kernel's LEVALREC at src/habu/habu2.f:2151-2152, which raises the count on a failed evaluate, dot habu-a-failed-evaluate-a8ed07fa); the E-TASK-COMPILE guards stay, since XREF:FORGET-DEFS-FROM reaches them. Depends: none. Claim: unassigned.
