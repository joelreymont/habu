---
title: GENIO slot state is carried in objects
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T12:43:53.452368+02:00"
closed-at: "2026-11-10T16:24:57.403836+02:00"
close-reason: "landed as 'Declare genio row state scratch' (genio-core, genio, log). test/file-objects.f L and L2 (a top level logs, the program logs) pass with the main task registering no release; N shows a device a top level left open is refused in the program. Open: the header routing cells (habu-show-a-built-c2292d3b); typed slot cells stay carried until typed scratch (habu-record-scratch-storage-4db7e41e). Gate at the landing tip (gate-all.sh): engine built twice equal before the signature, maps equal, test/pointer-storage-test.f and test/file-objects.f ok, suite 272 of 272, chain fixpoint at generation 2."
---

Problem (census habu-census-every-cell-c247785e, by reading, kinds inferred, not reproduced): a hook registered with IMAGE-LIFECYCLE:REGISTER cleans plain cells a file object carries, so a file whose top level reaches the registration and is then linked from its object gives the program the builder's state. Cells: src/lib/genio-core.f:310 (RESET-REGISTERED :288, SLOT-GEN :54, SLOT-STATUS :67, OP-* :44-51). Files: that one. Acceptance: a reproducer as a case of test/file-objects.f where a top level can reach the registration (or the report says why none can), failing first; then no file object carries these cells' process state, by the mechanism of design habu-design-no-file-298feb05. Verify: the case; the owner's focused test; engine built twice equal when the file is engine-provided. Depends: habu-design-no-file-298feb05. Ownership: the files named. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-o5.
