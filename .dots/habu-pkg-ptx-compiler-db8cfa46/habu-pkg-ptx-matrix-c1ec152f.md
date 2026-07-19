---
title: Package PTX matrix lowering
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:18:42.012333+02:00"
---

lib/ptx/cg-mma.f:64-1324 alone exposes 160 raw MMA-* words; cg-matmul.f, cg-matmul-emit.f, cg-matmul-naive.f, tile-pipe.f, cpp-slot.f, and related matrix helpers add lowering state, slot tables, pipeline bookkeeping, and emission stencils to the global dictionary. This is live Maki GEMM lowering, not test support. Give matrix lowering one explicit package boundary, with smaller reopened owners only where the existing CPP slot abstraction is independently public; keep capability checks, schedule state, accumulator/register maps, stencils, and builders private. Rename callers directly to short package tails; no forwarding globals. Preserve exact emitted PTX, register/resource counts, pipeline stage ordering, checked slot contracts, naive/MMA selection, numerical device results, and failure diagnostics. Add negative package fixtures for legacy MMA-/matrix helper globals and qualified private access, plus public API positives. Measure dictionary-name bytes, loaded JIT/DATA, CODELEN, generation latency, and current matrix kernel instruction/resource counts before/after; require shrink or a fully explained non-growth ledger. Verify matrix, slot, pipe, checked-negative, optimizer, ptx-stdlib, Maki matrix/device goldens, typed-local diff, package/host/filemap/dot lints, fixpoint, and full native gate. Parent: habu-pkg-ptx-compiler-db8cfa46; serialize public emitter renames after habu-pkg-ptx-emitter-6f1fa269.
