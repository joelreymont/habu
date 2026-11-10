---
title: Postgres owners carry process state in objects
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T12:43:53.430455+02:00"
closed-at: "2026-11-10T16:24:57.360136+02:00"
close-reason: "landed as 'Declare postgres registry state scratch' (pg.f, pg-rows.f). No build case of a top level that configures or connects. The scratch mechanism is shown once by test/file-objects.f R: a scratch cell, a SCRATCH-VARIABLE and a SCRATCH-BUFFER: one file's top level stored 7 into read 0 at a later file's top level and in the program. Suite pg passes. Gate at the landing tip (gate-all.sh): engine built twice equal before the signature, maps equal, test/pointer-storage-test.f and test/file-objects.f ok, suite 272 of 272, chain fixpoint at generation 2."
---

Problem (census habu-census-every-cell-c247785e, by reading, kinds inferred, not reproduced): a hook registered with IMAGE-LIFECYCLE:REGISTER cleans plain cells a file object carries, so a file whose top level reaches the registration and is then linked from its object gives the program the builder's state. Cells: src/lib/pg.f:784 (REGISTERED :146, CONFIGURED :118, CONN-CAPACITY :116, RESULT-CAPACITY :117); src/lib/pg-rows.f:226 (READERS-REGISTERED :67, READER-N :66). Files: those two. Acceptance: a reproducer as a case of test/file-objects.f where a top level can reach the registration (or the report says why none can), failing first; then no file object carries these cells' process state, by the mechanism of design habu-design-no-file-298feb05. Verify: the case; the owner's focused test; engine built twice equal when the file is engine-provided. Depends: habu-design-no-file-298feb05. Ownership: the files named. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-o3.
