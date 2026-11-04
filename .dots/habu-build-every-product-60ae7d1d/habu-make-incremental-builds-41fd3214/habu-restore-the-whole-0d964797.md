---
title: Restore the whole-file object codec
status: closed
priority: 1
issue-type: task
created-at: "2026-10-22T17:12:02.910649+03:00"
closed-at: "2026-10-28T17:13:33.379855+02:00"
close-reason: landed 999670ad, ad3f5c68, 322bfe28; ADMIT-FILE
---

Problem: the codec stores only definitions; created words, variables, constants, initialized data, writes to earlier data, slots, require rows and child loads have no stored form since 4dd64194/bb2f01fec229/b9749616f078. Acceptance: tools/native-unit-object.f packs one object per file: per-definition payloads in file order, RECORDS/PROTECTION/DATA/SLOTS/UPDATES/REQUIRES profiles recovered from those diffs, LOAD events, one checker export per file through the owner ABI, scope at exit, the frame's rows and input rows; ADMIT-FILE refuses a raw cell holding a code/dictionary/mapped address and names owner, offset and value; no range-history cells and no running chain hash. Verify: test/native-unit-file.f and a new object e2e asserting sections and the refusal diagnostic. Design: docs/architecture.md "Builds"; working notes ~/.cache/tmp/carl-file/design.md. Depends: frame dot. Claim: unassigned.
