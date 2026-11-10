---
title: AIO and curl record chains carry process state in objects
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T12:43:53.417816+02:00"
blocks:
  - habu-design-no-file-298feb05
---

Problem (census habu-census-every-cell-c247785e, by reading, kinds inferred, not reproduced): a hook registered with IMAGE-LIFECYCLE:REGISTER cleans plain cells a file object carries, so a file whose top level reaches the registration and is then linked from its object gives the program the builder's state. Cells: src/lib/aio.f:505 (MAPS-ARMED :257, AIO-RECS :246, AIO-GROUPS :247, REC-CAP :253, GEN-FLOOR :256); src/lib/net/curl.f:926 (MAPS-ARMED :782, CURL-RECS :776, REC-COUNT :780). Files: those two. Acceptance: a reproducer as a case of test/file-objects.f where a top level can reach the registration (or the report says why none can), failing first; then no file object carries these cells' process state, by the mechanism of design habu-design-no-file-298feb05. Verify: the case; the owner's focused test; engine built twice equal when the file is engine-provided. Depends: habu-design-no-file-298feb05. Ownership: the files named. Claim: unassigned.
