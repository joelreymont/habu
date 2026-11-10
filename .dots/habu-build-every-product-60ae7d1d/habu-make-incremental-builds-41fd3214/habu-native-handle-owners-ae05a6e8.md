---
title: Native handle owners carry process state in objects
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T12:43:53.405157+02:00"
closed-at: "2026-11-10T16:24:57.314509+02:00"
close-reason: "landed as 'Declare native handle state scratch' (f64-text, zip-ffi, zip, zip-state, font, signal). No build case per owner. The scratch mechanism is shown once by test/file-objects.f R: a scratch cell, a SCRATCH-VARIABLE and a SCRATCH-BUFFER: one file's top level stored 7 into read 0 at a later file's top level and in the program. Q shows an archive a top level left open is refused in the program (serials stay carried, habu-zip-font-and-6170fc0c). f64-text-test, zip-test, signal-test ok. Gate at the landing tip (gate-all.sh): engine built twice equal before the signature, maps equal, test/pointer-storage-test.f and test/file-objects.f ok, suite 272 of 272, chain fixpoint at generation 2."
---

Problem (census habu-census-every-cell-c247785e, by reading, kinds inferred, not reproduced): a hook registered with IMAGE-LIFECYCLE:REGISTER cleans plain cells a file object carries, so a file whose top level reaches the registration and is then linked from its object gives the program the builder's state. Cells: src/lib/f64-text.f:78 (REGISTERED :34, C-LOCALE :35); src/lib/zip-ffi.f:87 (REGISTERED :10); src/lib/font.f:168 (REGISTERED :100); src/lib/net/curl.f:529 (GLOBAL-REGISTERED :515, GLOBAL-DONE :514); src/lib/signal.f:304 (REGISTERED :104, STUB-A :94, CAUGHT :96, OWNER :99, READ-FD-CELL :97, WRITE-FD-CELL :98, READY :101). Files: those five. Acceptance: a reproducer as a case of test/file-objects.f where a top level can reach the registration (or the report says why none can), failing first; then no file object carries these cells' process state, by the mechanism of design habu-design-no-file-298feb05. Verify: the case; the owner's focused test; engine built twice equal when the file is engine-provided. Depends: habu-design-no-file-298feb05. Ownership: the files named. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-o1.
