---
title: AIO and curl record chains carry process state in objects
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T12:43:53.417816+02:00"
closed-at: "2026-11-10T16:24:57.340166+02:00"
close-reason: "landed as 'Declare aio record and group state scratch' and 'Declare curl record and global state scratch'. Q shows an aio ticket a top level kept is refused in the program (habu-an-aio-ticket-4bad2249). The scratch mechanism is shown once by test/file-objects.f R: a scratch cell, a SCRATCH-VARIABLE and a SCRATCH-BUFFER: one file's top level stored 7 into read 0 at a later file's top level and in the program. Not shown: a build whose top level ran a curl transfer (traced by reading: LOOP-START clears the fd table and the tickets are stored before any is read); the Linux aio backend was not run. Gate at the landing tip (gate-all.sh): engine built twice equal before the signature, maps equal, test/pointer-storage-test.f and test/file-objects.f ok, suite 272 of 272, chain fixpoint at generation 2."
---

Problem (census habu-census-every-cell-c247785e, by reading, kinds inferred, not reproduced): a hook registered with IMAGE-LIFECYCLE:REGISTER cleans plain cells a file object carries, so a file whose top level reaches the registration and is then linked from its object gives the program the builder's state. Cells: src/lib/aio.f:505 (MAPS-ARMED :257, AIO-RECS :246, AIO-GROUPS :247, REC-CAP :253, GEN-FLOOR :256); src/lib/net/curl.f:926 (MAPS-ARMED :782, CURL-RECS :776, REC-COUNT :780). Files: those two. Acceptance: a reproducer as a case of test/file-objects.f where a top level can reach the registration (or the report says why none can), failing first; then no file object carries these cells' process state, by the mechanism of design habu-design-no-file-298feb05. Verify: the case; the owner's focused test; engine built twice equal when the file is engine-provided. Depends: habu-design-no-file-298feb05. Ownership: the files named. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-o2.
