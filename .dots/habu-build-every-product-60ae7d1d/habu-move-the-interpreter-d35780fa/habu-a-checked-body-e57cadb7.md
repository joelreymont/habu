---
title: "A checked body's undefined word prints no file or line"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T22:35:50.361958+02:00"
---

Problem: an undefined word inside a checked body at tier 1 was reported to print `habu: in f: undefined word 'x'` with no file or line. That text is the checker's diagnostic (src/core/render.f:1782-1785); its DSITE (:905-911) writes `habu: in <name>: ` with no file when TOP-STEP is 0. Why a checked body reaches it with no top step is not known. Reproducer: under `1 set-tier`, a file holding `: f ( -- ) x ;` with x undefined, run with bin/hb --load. It is an acceptance case of its parent, habu-move-the-interpreter-d35780fa: fix in the code that stays, never in the machinery the parent replaces.
Acceptance: the cause is reported at file:line; then the reproducer's diagnostic, on a rebuilt hb, names the file and line.
Depends: none.
Claim: unassigned.
