---
title: Checker symbol table owns names
status: open
priority: 1
issue-type: task
created-at: "2026-06-28T16:55:56.882701+02:00"
---

Problem: src/core/checker.f still stores word names and package visibility as byte strings in USIG/DFER rows (USIG-NAME$, CHECKER-BUILD-PRIVATE/PUBLIC), so lookup/package metadata remains string-shaped after typed effect records landed. Fix: add checker-owned interned symbol ids with folded spelling + source spelling for diagnostics; records store symbol id and visibility/package fields, not encoded PKG:WORD strings. Acceptance: certified calls and defer lookup compare symbol ids; package public/private lookup remains correct; diagnostics still print source names; duplicate/candidate/undefine tests and full native gate pass.
