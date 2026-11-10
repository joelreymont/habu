---
title: "Persistent registrations and zip-state's fork hook under file objects"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T12:44:09.782502+02:00"
blocks:
  - habu-add-scratch-allot-07fcc86b
---

Problem (census habu-census-every-cell-c247785e, by reading, not reproduced): (1) the IMAGE-LIFECYCLE:REGISTER-PERSISTENT sites src/habu/t0-arm64.f:362 (LIVE :89), src/habu/t0-x64.f:154 (SINK-LIVE :43), src/lib/ffi-abi.f:786 (FN-REGISTERED :244) and src/lib/c2-memory/owner-runtime.f:78 keep a flag and a table entry, and an object carries both; whether a linked file's persistent hook is present in the program is not established. (2) src/lib/zip-state.f:50 registers a FORK-CHILD hook at top level, so a file linked from its object never registers it (inferred). (3) Not checked: whether TYPED-VARIABLE, TYPED-BUFFER and BUFFER: cells are carried; a TYPED-VARIABLE ptr cell marked scratch fails the build, rc 99 'snapshot address cell kind mismatch' (/Users/joel/Work/reviews/habu-10x-2026-11-10/hunt-process-cell.md). Acceptance: each of 1 to 3 shown by a mixed build (X, another program, X again in one out root) or shown harmless, then fixed by the mechanism of design habu-design-no-file-298feb05. Files: those named. Verify: cases of test/file-objects.f. Depends: habu-design-no-file-298feb05. Ownership: none yet. Claim: unassigned.
