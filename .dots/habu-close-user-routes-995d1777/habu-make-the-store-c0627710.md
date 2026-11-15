---
title: Make the store writers private to CHECK
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T19:39:41.033822+01:00"
---

Problem: the checker's store writers (E-ADD-EFFECT, E-BUILD-EFFECT, E-ADD-DELETED, RECOVERY-RECORD, USIGS-RESTORE-END, CHECKER-STORAGE-*, CHECKER-UNDEFINE, CHECKER-USIGS-TRUNCATE-FROM, CHECKER-REC-SYM, the CHECK:PRIM-* and READ-* seams, CHECK:FACTS!) are global or public. Acceptance: each is private to CHECK or its own package; each engine caller takes it by EXPORT in its private section; the global callers in xref.f, layout-buffer.f, render.f and verify-source.f are private words of their file's package. Depends: the EXPORT dot. Claim: unassigned.
