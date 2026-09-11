---
title: Read an IR view through one resolved scoped reader
status: closed
priority: 2
issue-type: task
created-at: "\"2026-09-11T16:38:06.256061+03:00\""
closed-at: "2026-09-11T18:50:28.288495+03:00"
close-reason: "Landed on the root at 43e272e2: reader API (sealed IR-ARENA:reader, OPEN/OPEN-LIVE, RD@/RD-SIZE revalidating generation and state per read, eager RETIRE-CHILDREN at context teardown) and the migration of source, op, fun, symbol, schema, verify, native/tape, type and attr; 26.9 M resolutions on one ANSWER-COUNT compile became 4.3 M opens; replay onto the root byte-identical, no stack-only reds, 154 suites pass both sides. Measured ceiling: stripping all per-read validation would gain only 5.5% on the single word, so the pinned raw-read experiment is shelved with its numbers (rowan-arena/tmp). Timing pair on the root pending a quiet machine."
---

Problem: dialect field readers resolve the same view once per field: src/compiler/ir/op.f FLD does ROW-AT plus RC@ per field and SPAN@ four reads; fun.f, type.f, symbol.f, schema.f, attr.f, source.f, verify.f, native/tape.f, native/hir-word.f have the same shape; SIZE alone is 346 M calls per load. The IR design's per-read validation obligation is the cause. Acceptance: arena.f gains a scoped reader (resolve a view once, read many ordinals, fail-closed at scope entry, no public pointer, retirement inside the scope impossible under the single-task discipline or re-validated cheaply, the choice stated); docs/compiler-ir-design.md amends the validation obligation to per-scope; every listed caller migrates with call counts before and after; controlled pair. Files: src/compiler/ir/arena.f, docs/compiler-ir-design.md, the caller files above and their tests. Verify: the ir-* suites and the forced-tier Tender pair. Depends: habu-resolve-an-ir-20006290. Ownership: rowan (API), caller files by maintainer grant. Claim: unassigned


Current ownership and handoff: Caller grant accepted by Rowan after the flat-read controlled pair and explicit lifetime contract. Rowan arena lane owns arena/context API and op.f, fun.f, type.f, symbol.f, schema.f, attr.f, source.f, verify.f, native/tape.f migration; hir-word.f read migration is inside Rowan reuse lane. API and callers remain separately reviewable.
