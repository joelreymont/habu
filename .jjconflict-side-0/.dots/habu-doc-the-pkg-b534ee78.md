---
title: Document the package-gate admission categories
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:25:26.325034+02:00"
---

Full context: from agent mirrorcat 2026-07-30. docs/forth.md section Packages lists the package-gate exceptions but stops at the core-surface entries: the engine-trunk category (src/habu/habu2.f and src/habu/layout.f body-plus-old-name admissions, commits a943eb40/13bb611a/f8d18900) and the Gforth mirror category (bootstrap/cg/forth.fs, commit 2cceebce) are both absent. One doc leaf covering all categories together: for each, the structural argument (what makes packaging impossible or deferred there), the exact paths, what is still reported (new trunk globals, whole-file replacement), the owning authority for the admitted surface (parity gates for the mirror, retirement dots for the trunk), and the retirement condition. Keep it in the section the gate's own comments already reference so the list cannot drift silently - and add a line to the gate test that pins the doc mentions the categories (structure-checked, not substring, per Test Integrity).
