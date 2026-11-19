---
title: Name a declared row in the inferred line
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T14:36:49.413784+02:00"
---

Problem: the checker's inferred line prints a declared row as [ -- ]: a word declared R [ R -- R ] -- R n is shown as [ -- ] -- n n (the staged host 27bc419d does it too). src/core/render.f renders rows (RROW-LETTER, RBUF-ROW). Acceptance: the inferred line names each row as its declaration does, and a test shows it. Depends: none. Claim: unassigned.
