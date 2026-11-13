---
title: "Persistent registrations and zip-state's fork hook under file objects"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T12:44:09.782502+02:00"
closed-at: "2026-11-10T16:24:57.484512+02:00"
close-reason: "landed as 'Drop the persistent capture hooks' (t0-arm64, ffi-abi, c2-memory/owner-runtime; t0-x64 edited by reading only, nothing built for x86-64). RETIRED-COUNT stays carried with the rows it counts. Not shown: the mixed builds of 1 to 3; that an engine-header staged buffer carries nothing into a build. FFI's forget path is habu-ffi-s-forget-de7d255a. test/boot-relocation-e2e.f and test/c2-memory-runtime.f ok. Gate at the landing tip (gate-all.sh): engine built twice equal before the signature, maps equal, test/pointer-storage-test.f and test/file-objects.f ok, suite 272 of 272, chain fixpoint at generation 2."
---

Problem (census habu-census-every-cell-c247785e, by reading, not reproduced): (1) the IMAGE-LIFECYCLE:REGISTER-PERSISTENT sites src/habu/t0-arm64.f:362 (LIVE :89), src/habu/t0-x64.f:154 (SINK-LIVE :43), src/lib/ffi-abi.f:786 (FN-REGISTERED :244) and src/lib/c2-memory/owner-runtime.f:78 keep a flag and a table entry, and an object carries both; whether a linked file's persistent hook is present in the program is not established. (2) src/lib/zip-state.f:50 registers a FORK-CHILD hook at top level, so a file linked from its object never registers it (inferred). (3) Not checked: whether TYPED-VARIABLE, TYPED-BUFFER and BUFFER: cells are carried; a TYPED-VARIABLE ptr cell marked scratch fails the build, rc 99 'snapshot address cell kind mismatch' (/home/joel/Work/reviews/habu-10x-2026-11-10/hunt-process-cell.md). Acceptance: each of 1 to 3 shown by a mixed build (X, another program, X again in one out root) or shown harmless, then fixed by the mechanism of design habu-design-no-file-298feb05. Files: those named. Verify: cases of test/file-objects.f. Depends: habu-design-no-file-298feb05. Ownership: none yet. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-o7.
