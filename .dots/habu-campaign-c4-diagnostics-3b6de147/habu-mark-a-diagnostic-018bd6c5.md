---
title: "Mark a diagnostic's cut of a long row"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T10:18:23.848210+01:00"
---

Problem: src/core/render.f RBUF-ROW (:269-285) shows a diagnostic (RNAMES 0) only a row's top RBUF-CAP (64) values and marks nothing, so a rejection of a call passing a 64-cell quotation where 100 cells are expected prints both types as the same 64 cells: '[: drop x64 ;] P2-TAKE' against a 100-cell EFFECT:READ-TYPE input, or a written ( [ n x100 -- ] -- ) signature. The nesting cut (QDEPTH-CAP) marks itself with EMIT-UNSAID. Acceptance: a diagnostic that cuts a row says so and how many values it left out, and the expected and found types of such a rejection differ in what it prints. Depends: none. Claim: unassigned.
