---
title: "Convention: scope pairs are FOO ... ;FOO tree-wide"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T18:09:37.298229+02:00"
---

User decision 2026-07-04: every scope/pairing word uses the form FOO opened, ;FOO closed (closer mirrors the opener with a ; prefix, same case). Inventory and rename ALL pairs: (1) package ... end-package -> package ... ;package - ENGINE-LEVEL (parsed in src/habu/habu2.f) and appears in essentially every .f file: this is a tree-wide mechanical rename + engine change + fixpoint/seed refresh; MUST execute after the TFAM branch merges (their campaign touches package machinery - sealed packages - and every file; doing it mid-flight collides branch-wide). (2) BEGIN-STRUCTURE/END-STRUCTURE -> STRUCTURE/;STRUCTURE (dot habu-core-structures-dsl-609be5a9 carries specifics). (3) Inventory others: TEST:SUITE/TEST:END-SUITE and any BEGIN-*/END-* or *-BEGIN/*-END pairs (rg for END- closers) - list each in the execution plan with its owner. Keep ; (definition closer) and ;CODE-style engine words distinct - only SCOPE pairs rename. Update docs/forth.md conventions section in the same change. Execution: one lane per pair class, full gates (engine refresh + run.f + maki + zed), sequenced after TFAM merge; coordinate with the TFAM session so their new SUMTYPE/PRODUCT/ENUM closers follow the same convention FROM BIRTH (tell them via a note in their PLAN.md or this dot).
