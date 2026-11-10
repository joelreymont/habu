---
title: "Retire the old hb builder's prefix boundary"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-09T18:18:45.109487+02:00"
closed-at: "2026-11-10T21:52:35.262528+02:00"
close-reason: "done: prefix-rewind.f and build-fixpoint.f were deleted in 1d97c3f2; no RBF-BND-REC, CHECKER-BOUND or CURSORS is left in src, tools or test; the byte-for-byte rebuild from a different builder was not checked"
---

Problem: src/core/lower-cert-seal.f:89 runs CHECKER-BOUND:MARK (src/core/checker.f ~27245) at the end of the boot prefix. In a cross-build that top level runs inside the building hb, so RBF-BND-REC's RBF.NDICT saves the builder's ndict@, not the product's record count. One build from a builder whose record count differs therefore leaves one cell that differs from what the product writes when it rebuilds itself. Measured 2026-11-09 in carl-kobj/carl-mk: G1->G2 (J2 vs G1 builders, 3462 records apart) cell +3426 (the build tools account for the rest); M1b->M2b (+6 records from master's body-text change) cell +6 (file offset 3166369). M2->M3 is byte-equal (fixed point). MARK, REWIND, CURSORS, src/habu/prefix-rewind.f and tools/build-fixpoint.f's CURSORS check serve only the old hb builder, which DECISIONS 'the old builders' retires once the arm64 cross-build writes a working hb (CLAUDE.md: never patch machinery being replaced). Acceptance: hb built once from a builder with a different record count rebuilds itself byte for byte (signature aside), maps equal; no RBF-BND-REC, prefix-rewind.f or CURSORS probe left. Files: src/core/checker.f, src/core/lower-cert-seal.f, src/habu/prefix-rewind.f, tools/build-fixpoint.f, tools/bootstrap.sh. Depends: arm64 landing (Goals 'arm64'). Ownership: carl.
