---
title: Delete the already-reported machinery
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T16:00:42.384409+01:00"
---

Problem: REFUSAL-ABI:CODE-CELL (src/habu/layout.f, src/habu/data-claims.f, src/habu/process-fork.f, its compare in src/habu/habu2.f), REPORTED-ERROR?, CAST-ERROR:SKIP's list, PINNED?, JSON-REPORTED and the THREW? list tests exist only to guess whether a code was printed. Acceptance: all are deleted; every throw left in src/core, src/habu and src/compiler is a refusal, a 76 die or a control code with a named catcher in its package. Depends: habu-move-the-diagnostic-783e0965. Claim: unassigned.
