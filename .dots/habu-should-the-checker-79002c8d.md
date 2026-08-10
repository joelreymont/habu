---
title: Should the checker refuse a silent global redefinition
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T22:06:16.610713+02:00"
---

src/core/generated-declaration.f defines : ARM-BASE (a colon word) while asm.f used to define variable ARM-BASE, generated-declaration loading first - a silent global redefinition the packaging incidentally removed (pkgasm 2026-08-11). Duplicate definitions of one global across two files with different shapes went unrefused. Decide: should the checker (or a lint) refuse a global redefinition whose shape differs, and is there another instance in the tree (sweep). Files: probe first. Depends: none.
