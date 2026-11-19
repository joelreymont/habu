---
title: "Keep a build program's saved rejection record in hb"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T16:20:19.796819+01:00"
---

Problem: a build of `: W ( -- ) s" " SOURCE-ROOT:CANON-OS drop 2drop ; W` prints only 'hb-build: ... threw 7134 E-PATH-RANGE' (rc 67), while `bin/hb s.f` prints the error[E-PATH-RANGE] record. Inferred cause: the program's compiled src/core/rejection.f SAVE keeps the record in its own block (rejection.f:115-119 MAIN-BLOCK) while PASS stamps hb's STAMP-CELL, so hb's interpreter LEFT finds no saved record of its own. Same split as dot habu-reject-a-build-1499f333 (two copies of rejection state during a build). Acceptance: that build prints the E-PATH-RANGE record and nothing else; test/file-objects.f holds the case. Depends: habu-reject-a-build-1499f333. Claim: unassigned.
