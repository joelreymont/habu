---
title: Make a BUFFER record without per-cell items
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T11:28:36.724772+01:00"
---

Problem: MAKE on a STRUCTURE with a BUFFER field takes ceil(N/CELL) n items the program writes by hand, a count that moves with the named constant and the target's cell size (BUFFER count review, carl-bufcount); its only caller is test/structure-decl-suite.f. Acceptance: no program states a count that depends on the cell size to make a BUFFER record. Depends: the BUFFER count feature (habu-structure-s-buf-cb3240cc) lands first. Claim: unassigned.
