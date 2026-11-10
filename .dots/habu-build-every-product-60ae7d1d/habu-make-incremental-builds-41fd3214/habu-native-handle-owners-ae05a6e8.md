---
title: Native handle owners carry process state in objects
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T12:43:53.405157+02:00"
blocks:
  - habu-design-no-file-298feb05
---

Problem (census habu-census-every-cell-c247785e, by reading, kinds inferred, not reproduced): a hook registered with IMAGE-LIFECYCLE:REGISTER cleans plain cells a file object carries, so a file whose top level reaches the registration and is then linked from its object gives the program the builder's state. Cells: src/lib/f64-text.f:78 (REGISTERED :34, C-LOCALE :35); src/lib/zip-ffi.f:87 (REGISTERED :10); src/lib/font.f:168 (REGISTERED :100); src/lib/net/curl.f:529 (GLOBAL-REGISTERED :515, GLOBAL-DONE :514); src/lib/signal.f:304 (REGISTERED :104, STUB-A :94, CAUGHT :96, OWNER :99, READ-FD-CELL :97, WRITE-FD-CELL :98, READY :101). Files: those five. Acceptance: a reproducer as a case of test/file-objects.f where a top level can reach the registration (or the report says why none can), failing first; then no file object carries these cells' process state, by the mechanism of design habu-design-no-file-298feb05. Verify: the case; the owner's focused test; engine built twice equal when the file is engine-provided. Depends: habu-design-no-file-298feb05. Ownership: the files named. Claim: unassigned.
