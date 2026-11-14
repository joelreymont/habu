---
title: Refuse xref-retarget at DICT-CAP
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T04:50:14.751413+02:00"
---

Problem: src/habu/habu1.f BXREFRETARGET (:3544) refuses an index only past NDICT ('9 NDICT CMP,  C-HI bad BCOND,'). NDICT == DICT-CAP is valid when the dictionary is full (habu1.f:1628), and slot DICT-CAP is the control-flow band (habu2.f:3631), so at a full dictionary xref-retarget writes [0] and [8] of that slot: the CF depth counter and the first CF record. Found by the record-facts! review 2026-11-07, which gave BRECFACTS the guard. Acceptance: '14 DICT-CAP LIT64,  9 14 CMP,  C-CS bad BCOND,' after the NDICT test, as BRECFACTS and BNDAPPEND (:1674) have. Files: src/habu/habu1.f. Verify: engine build; bin/hb --load test/run.f -- engine prim-parity. Depends: none. Ownership: src/habu/habu1.f BXREFRETARGET. Claim: unassigned.
