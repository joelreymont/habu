---
title: Read IR record cells once per row in tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T17:04:47.210710+03:00"
---

Problem: the tier-1 compiler spends 22.3 percent of corpus compile time in IR-ARENA:RD@ plus the ptr-field and cell-view calls it makes, 5.5 percent in RD-SIZE and 9.1 percent in the span guard under `!` (internal-profiler measurement, compile-floor lane, 2026-09-16, docs/compiler-measurements.md profile section at 3dca9e49): every access to a record cell re-derives the row pointer and revalidates headers (RHDR-CK, BHDR-CK, PHDR-CK, FROZEN-READER) on top of what RD@ already checks, so the compiler reads far more cells than the IR contains. Deleting those words outright would still leave about 1.6 ms per corpus word against the 0.8 ms target of habu-cut-the-fixed-171aebb2, so the fix is a design change, not a removal: resolve a row once into a cached, typed frame (base pointer plus size, validated at resolution) and read its cells through that frame; keep header validation at frame creation and at every mutation, never per read. Acceptance: the frame type and its accessors in src/compiler/ir/arena.f (or a new file beside it), the hot readers in elaborate.f, select.f, regalloc.f, spill.f and schema.f converted, the verifier untouched in strength; corpus compile time per word before and after with tools/tier-bench.f (target under 0.8 ms/word, report what was reached and why), the compile-floor number, engine byte fixpoint, full gate green. Files: src/compiler/ir/arena.f, context.f, schema.f, src/compiler/native/*.f. Verify: tools/tier-bench.f; tools/native-build.f fixpoint with timing; test/run.f. Depends: the a+b+c commits of habu-cut-the-fixed-171aebb2 land first (same files). Ownership: IR access. Claim: unassigned.
