---
title: Find why a refused body thrown out of a build sets bit 62 of a header cell
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T23:20:37.939713+02:00"
---

Problem: with S5b1b's WITHOUT-PACKAGE removed (the old LINKER:WITH), test/linker.f case k's body is refused rc 70 and its throw out of the build leaves hb's DATA header cell at offset $169638 with bit 62 set (HOST-CHECK, HEADER-DIFFS 1, /tmp/jerry-s5b1b/mutation.log). The old WITH never touched package cells, so another store leaks on that throw path. Acceptance: name the cell and its owner; a case in test/linker.f whose build body the checker refuses (e.g. a definition with a wrong effect, under the new WITH) shows HOST-CHECK clean, or the owner's restore is fixed. Files: test/linker.f, src/habu/linker.f, the owner of the cell. Verify: bin/hb --load test/run.f -- linker. Depends: S5b1b landed. Ownership: jerry. Claim: unassigned.
