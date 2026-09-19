---
title: Cover a does> row that is one open-width instance
status: open
priority: 3
issue-type: task
created-at: "2026-09-19T18:27:53.940464+03:00"
---

Problem (adversarial review of 77fe4a87..fba695f8, minor): with the does> glue landed, src/compiler/native/compiler.f CHECK-DOES-SPLIT no longer refuses DOES-WIDE?, so a does> row whose ONLY term is an open-width instance (does> ( -- box<t> ) where box's width reads t) takes dict.f ROW-GLUE's 'terms 1 = if cells GLUE-WHOLE' arm with cells = ROW-CELLS' registry guess (the open argument counted as one cell) instead of E-NELAB-BUNDLE; the fixture C-BOX-DEF: covers only the din side (terms 2 <> cells). CHECKER-OWNER:DOES-WIDE? and checker.f CD-WIDE are written but no longer read by the compiler. Acceptance: a fixture with a does> clause yielding only an open-width instance value (find or build a checkable body; if none can exist, prove it and say why in the comment) pinned to the refusal the definition rule gives (E-NELAB-BUNDLE by name, never a guessed placement); DOES-WIDE?/CD-WIDE either read by that path or removed with their ABI slot retired (checker-owner-abi.f, OWNER-SIZE-AGREE) - no dead readers. Files: src/compiler/native/dict.f, compiler.f, checker-owner.f, src/core/checker.f, checker-owner-abi.f, test/compiler/native-rename-rows.f. Verify: the fixture; three generations with cmp; test/run.f. Depends: none. Ownership: native chain. Claim: unassigned.
