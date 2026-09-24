---
title: Persist registry arrays at fill rather than capacity
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T13:08:16.440507+03:00"
---

Problem: REG-PERSIST-BUF in the checker persists its callers' arrays at declared capacity, not fill (TFAM-SNAPSHOT-PERSIST, SCHEMA-SNAPSHOT-PERSIST, CT-, VREC- and SYM-SNAPSHOT-PERSIST), unlike the string pools and USIGS/NORET which round to the live size. It costs nothing today because no such store has outgrown its boot buffer, but VRN (capacity 16,384 records, 1 live) would bake 1,179,648 bytes the first time it does and VNARG 131,072 for zero records (census 2026-09-16, tools/data-table-census.f). Acceptance: REG-PERSIST-BUF persists the fill (rounded to whole records) and the restore sizes the mapping from the persisted length, a fixture grows one registry past its boot buffer and proves the image carries only the fill, byte fixpoint. Files: src/core/checker.f (REG-PERSIST-BUF and its callers), the checker snapshot fixtures. Verify: the fixture and the census. Depends: habu-qualify-habu-for-9ccd0432 (after release). Ownership: hazel line. Claim: unassigned.
