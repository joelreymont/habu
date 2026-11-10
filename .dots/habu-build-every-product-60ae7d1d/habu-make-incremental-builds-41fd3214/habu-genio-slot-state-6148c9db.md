---
title: GENIO slot state is carried in objects
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T12:43:53.452368+02:00"
blocks:
  - habu-add-scratch-allot-07fcc86b
---

Problem (census habu-census-every-cell-c247785e, by reading, kinds inferred, not reproduced): a hook registered with IMAGE-LIFECYCLE:REGISTER cleans plain cells a file object carries, so a file whose top level reaches the registration and is then linked from its object gives the program the builder's state. Cells: src/lib/genio-core.f:310 (RESET-REGISTERED :288, SLOT-GEN :54, SLOT-STATUS :67, OP-* :44-51). Files: that one. Acceptance: a reproducer as a case of test/file-objects.f where a top level can reach the registration (or the report says why none can), failing first; then no file object carries these cells' process state, by the mechanism of design habu-design-no-file-298feb05. Verify: the case; the owner's focused test; engine built twice equal when the file is engine-provided. Depends: habu-design-no-file-298feb05. Ownership: the files named. Claim: unassigned.
