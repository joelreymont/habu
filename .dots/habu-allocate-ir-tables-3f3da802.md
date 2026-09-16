---
title: Allocate IR tables from one mark-release scratch per word
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T17:08:28.525296+03:00"
---

Problem: src/compiler/ir/arena.f is already a bump allocator (cells come from IR-CTX:SCRATCH-TAKE, a context mapping released whole at teardown), which is the Forth HERE/ALLOT discipline applied to a transient region; what costs the compile floor is everything wrapped around it: about 17 arenas per module, a 64-slot registry with generation-checked handles and two scans per creation, geometric growth by copy per table, the interner cloned per module, and per-access header revalidation (compile-floor profile, 2026-09-16, 3dca9e49). The design doc itself says the linear-ownership work removes the handle mechanism. Joel: Forth has HERE and ALLOT; why is that insufficient for the compiler? Answer: it is sufficient for the lifetime, the IR only needs one scratch region marked at word start and released at word end, separate from the dictionary because the word's own dictionary allocations made in the same window must persist; what must stay is typed index addressing per table (no raw pointers, canonical digest, the Rocq model). Acceptance: one scratch region per compilation with mark and release; tables are index-addressed slices of it sized from the measured high-water of the self-build and the corpus (named constants with the marks) and refuse growth by name; the registry, generations and per-creation scans go (a stale handle cannot exist when a region outlives every handle into it: prove it by construction and delete E-IR-ARENA-STALE if nothing can raise it); the per-module interner clone becomes a borrow of the dialect's frozen vocabulary; compile-floor and corpus numbers before and after with tools/tier-bench.f; the freeze verifier and canonical digest unchanged; byte fixpoint; full gate green. Files: src/compiler/ir/arena.f, context.f, symbol.f, fun.f, op.f, attr.f, type.f, schema.f, source.f, verify.f, src/compiler/native/tape.f, hir-word.f, docs/compiler-ir-design.md sections 6.2-6.3 and 10.2. Verify: tools/tier-bench.f; tools/native-build.f fixpoint with timing; test/run.f. Depends: the a+b+c commits of habu-cut-the-fixed-171aebb2; supersedes its interner-borrow step if this lands first. Ownership: IR memory. Claim: unassigned.
Design target restated 2026-09-16 18:40 (Joel: Forth solved this; check how
Forth handles data and do the same). The rest of Habu already does: the
checker keeps 107 create/allot tables, the engine has here/allot/, as
primitives, the prefix rewind is a MARKER. Only src/compiler/ir invented a
context with mmap chunks, 17 growable arenas per module, a 64-slot registry and
generation-stamped handles. The Forth shape for the IR: one scratch region
with its own pointer (SCRATCH-HERE), SCRATCH-MARK at word start, SCRATCH-ALLOT
for every table as a slice returning an index base, SCRATCH-RELEASE at word
end; passes hand offsets, not handles; nothing is freed before the end of the
word, so nothing can be stale during it and no generation is needed; sizes are
named constants from measured high-water marks with a named refusal; the
checker's nominal index types (op index vs block index) stay as compile-time
types. The generation seal is kept only if a handle provably escapes a word's
compilation (NCOMP keeps three modules live across passes: they become three
offset ranges in the same region). Follows the interner/schema borrow and the
build-once fusion; the liveness proof is the work, written into
docs/compiler-measurements.md by the borrow commit.
