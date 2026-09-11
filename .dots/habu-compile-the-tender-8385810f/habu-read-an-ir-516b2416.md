---
title: Read an IR view through one resolved scoped reader
status: open
priority: 2
issue-type: task
created-at: "2026-09-11T16:38:06.256061+03:00"
---

Problem: dialect field readers resolve the same view once per field: src/compiler/ir/op.f FLD does ROW-AT plus RC@ per field and SPAN@ four reads; fun.f, type.f, symbol.f, schema.f, attr.f, source.f, verify.f, native/tape.f, native/hir-word.f have the same shape; SIZE alone is 346 M calls per load. The IR design's per-read validation obligation is the cause. Acceptance: arena.f gains a scoped reader (resolve a view once, read many ordinals, fail-closed at scope entry, no public pointer, retirement inside the scope impossible under the single-task discipline or re-validated cheaply, the choice stated); docs/compiler-ir-design.md amends the validation obligation to per-scope; every listed caller migrates with call counts before and after; controlled pair. Files: src/compiler/ir/arena.f, docs/compiler-ir-design.md, the caller files above and their tests. Verify: the ir-* suites and the forced-tier Tender pair. Depends: habu-resolve-an-ir-20006290. Ownership: rowan (API), caller files by maintainer grant. Claim: unassigned
