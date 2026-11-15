---
title: "Give back a long name's code bytes when a link throws"
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T03:14:45.796440+02:00"
---

Problem: if a build's first counted definition has a long name and its link throws after INTERPRETER:DEF-RECORD (src/habu/linker.f MAKE-DEF :4744, DEF-RECORD :4767) but before the record is counted (ndict-append), for example E-NPUB-ROOM from NPUB:IMPORT-PENDING, the name's bytes def-open wrote out of line (src/habu/prims.f:565-566) stay in hb's code arena after the build closes: FORGET-SPACE (linker.f:5709) takes its floor from CODE-RECLAIM:FLOOR-FROM (src/habu/xref.f:719), which sees only counted records, a package record's REC-LOW (xref.f:681) is 0, and DEF-ABORT (LINK-DEF, linker.f:4822) leaves CP where it is. A compiled file avoids it because FRAME-ROLLBACK-CODE cuts CP (src/habu/interpreter.f:2715). Found by reading, not reproduced. The only known trigger is NPUB:IMPORT-PENDING's E-NPUB-ROOM (hb's code arena full), so a reproducer costs a nearly full arena. The leak matters only to a process that goes on building. Acceptance: reproduce first (a fixture whose first definition has a long name and whose link throws before the count); then LINK-DEF restores CP to its value before DEF-RECORD on such a throw, so hb's code arena after the failed build ends where it began. Depends: a cheap trigger, or a rework of the link's failure path. Claim: unassigned, owner carl.
