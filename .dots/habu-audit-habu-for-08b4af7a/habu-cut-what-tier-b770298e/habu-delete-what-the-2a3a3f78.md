---
title: Delete what the NDICT cleanups left uncalled
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T18:08:07.646116+01:00"
---

Problem: deleting the does> glue, the owner record's unread fields and NDICT's wrappers left words no caller reaches: the checker's does> N and SLOT readers, four CHECKER-OWNER operations with their defers, NDICT's EXEC-CELLS and FINALLY-CELLS aliases, a public GLUE-NONE only dict.f uses, and dict.f and checker-owner.f comments about readers and a pass 2 that no longer exist. Acceptance: each is deleted, made private or made true; the engine builds and its link and parity tests pass. Depends: none. Claim: unassigned.
