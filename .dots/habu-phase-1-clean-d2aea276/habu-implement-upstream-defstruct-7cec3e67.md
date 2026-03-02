---
title: Implement upstream defstruct options
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.152041+02:00"
blocks:
  - habu-separate-structure-70df2f68
---

Problem: upstream defstruct options such as :type list, :named, and :print-function are incomplete. Acceptance: used upstream options work with correct representation and printer behavior. Files: lib/stdlib.habu:6198-6289; ../maxima/src/trans5.lisp:78; ../maxima/share/affine/sparsemat.lisp:10-37; ../maxima/src/numth.lisp:1810-1814. Verify: focused upstream defstruct examples load and print correctly. Blockers: habu-separate-structure-70df2f68.
