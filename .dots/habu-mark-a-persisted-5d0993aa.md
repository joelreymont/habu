---
title: "Mark a persisted record's pointer fields for the snapshot"
status: open
priority: 2
issue-type: task
created-at: "2026-09-18T10:24:05.096073+03:00"
---

Problem: a persisted arena's pointer cells are marked by hand with ptr-cell-mark per cell (src/core/checker.f SYM rows :9399-9410, src/compiler/native/string.f NSTR pool owner, src/compiler/ir/arena.f descriptor), so a declared record inside a persisted arena has no way to have its pointer fields marked from its declaration. Acceptance: a declared record in a persisted arena has each pointer field marked exactly once from the family's field schema; a restored image reads them back; a scalar field is not marked; a record whose pointer column became a declared table is marked once per table; fixture. Files: src/core/checker.f or the snapshot owner (src/habu/aot-closure.f, src/habu/address-cells.f), test/. Verify: the fixture; fixpoint; test/run.f. Depends: habu-generate-typed-field-ba63866e. Ownership: snapshot. Parent: habu-campaign-c2-mem-c3d7662b. Claim: unassigned.
