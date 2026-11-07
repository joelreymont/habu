---
title: "Give back a long name's code bytes when a link throws"
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T03:14:45.796440+02:00"
---

Problem: if a build's first counted definition has a long name and its link throws after OUTER:DEF-RECORD (src/habu/linker.f MAKE-DEF, about :1660) but before the record is counted (ndict-append), for example E-NPUB-ROOM from NPUB:IMPORT-PENDING, the name's bytes def-open wrote out of line (src/habu/prims.f:668-669) stay in hb's code arena after the build closes: FORGET-SPACE (linker.f about :1970) takes its floor from CODE-RECLAIM:FLOOR-FROM (src/compiler/native/xref.f:741), which sees only counted records, a package record's REC-LOW is 0 (xref.f:687-701), and DEF-ABORT leaves CP where it is. A compiled file avoids it because FRAME-ROLLBACK-CODE cuts CP (src/habu/interpret.f:259-260). Found by the L4 wording re-review (2026-11-07), from reading, not reproduced. Acceptance: reproduce first (a fixture whose first definition has a long name and whose link throws before the count); then LINK-DEF restores CP to its value before DEF-RECORD on such a throw, so hb's code arena after the failed build ends where it began. Files: src/habu/linker.f, test/file-objects.f or a fixture under test/fixtures/file-objects/. Verify: the reproducer, then the four suites linker arm64-link-data arm64-link-run file-objects. Depends: none. Ownership: carl, worker lane. Claim: unassigned.
