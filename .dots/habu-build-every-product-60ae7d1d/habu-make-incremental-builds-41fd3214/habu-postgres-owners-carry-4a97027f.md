---
title: Postgres owners carry process state in objects
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T12:43:53.430455+02:00"
blocks:
  - habu-design-no-file-298feb05
---

Problem (census habu-census-every-cell-c247785e, by reading, kinds inferred, not reproduced): a hook registered with IMAGE-LIFECYCLE:REGISTER cleans plain cells a file object carries, so a file whose top level reaches the registration and is then linked from its object gives the program the builder's state. Cells: src/lib/pg.f:784 (REGISTERED :146, CONFIGURED :118, CONN-CAPACITY :116, RESULT-CAPACITY :117); src/lib/pg-rows.f:226 (READERS-REGISTERED :67, READER-N :66). Files: those two. Acceptance: a reproducer as a case of test/file-objects.f where a top level can reach the registration (or the report says why none can), failing first; then no file object carries these cells' process state, by the mechanism of design habu-design-no-file-298feb05. Verify: the case; the owner's focused test; engine built twice equal when the file is engine-provided. Depends: habu-design-no-file-298feb05. Ownership: the files named. Claim: unassigned.
