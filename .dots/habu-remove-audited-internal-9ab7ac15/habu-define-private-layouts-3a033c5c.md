---
title: Define private layouts once
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.726409+03:00"
---

Problem: the type layer checks its own private record layouts at run time and keeps metadata that only those checks read, in src/core/type-schema.f, src/core/type-family.f, the checker's private record layouts (src/core/checker.f) and their copies in test/engine-suite.f; the original names no site. One found by reading: a product field's row (ADD, src/core/type-family.f:2567-2590) stores a byte offset, a byte count, an alignment and flags beside its slot and cell count, and PF-LAYOUT-REQUIRE (:2533-2549) refuses any row where they are not slot cells, cells cells, CELL and 0 for every layout policy it admits (the niche, boxed and custom policies are refused outright). src/core/sumtype.f:1975-1978 reads the byte offset. docs/forth.md "Structures And Enums" (:430) is the prescription to update. Keep constants and accessors as the one source, validation of the real target width and of saved formats, and the storage and recovery tests.

Acceptance: a census in this dot of every such self-check and the metadata only it reads, at file:line, each marked removed or kept with its reason; the removable ones removed, any saved-format field brought to the user before it changes; docs/forth.md "Structures And Enums" matches.

Files: src/core/type-schema.f, src/core/type-family.f, src/core/checker.f, src/core/sumtype.f, test/engine-suite.f, docs/forth.md.

Verify: test/enum-decl-suite.f and test/c2-records-types.f pass; the engine builds with `bin/hb --load tools/hb-build.f -- --engine -o $OUT/hb`; the convergence check (docs/bootstrap.md "Convergence check") if a built layout changes.

Depends: none.

Ownership: unassigned. Claim: unassigned.
