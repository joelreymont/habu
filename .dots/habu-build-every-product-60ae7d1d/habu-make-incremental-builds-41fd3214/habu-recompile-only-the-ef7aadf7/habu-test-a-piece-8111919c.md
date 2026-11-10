---
title: Test a piece that holds a header-data address
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T12:17:33.248636+02:00"
---

Problem: HOLDS-HEADER's true branches (linker.f WALK-STORERS taking header pairs, the -1 REACHED row, REACHED-CK's header case) run in no test: no fixture piece holds a tag-4 reference. The review of xqvqnuts found one way to make one: a string literal with the bytes of an owned primitive's facts text gets that interned body (OWNED-RECORDS, intern-table.f:214), which lies in [DATA-START, OPEN-DP). Acceptance: a file-objects sequence where a file holding a header-data address and running a word compiles again when an earlier file that stores into header data changes, or a recorded reason the fixture cannot make such a piece. Files: test/file-objects.f, test/fixtures/file-objects/build.f. Verify: file-objects. Depends: habu-drop-header-stores-02065287 fold-ins landed in carl-words. Ownership: carl. Claim: unassigned.

Lane (carl, 2026-11-10): workspace carl-hh, brief .out/briefs/lane-header-address-test.md.

2026-11-10: merged into the stack as opmoosvq "Test a piece that holds a header address" (carl-hh retired): the held sequence prints `held: compiled 2 2 0`; r.f's literal `( -- ) primitive ` takes the interned facts text of an owned primitive, so DATA-REF+ writes a tag-4 reference. FILES-C 37. Run under the pre-A2b engine only; lane B's file-objects run covers it on the tip.
