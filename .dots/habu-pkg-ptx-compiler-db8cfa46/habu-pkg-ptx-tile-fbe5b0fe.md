---
title: Package PTX tile lowering
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:19:13.795286+02:00"
---

lib/ptx/tile.f, tile-loop.f, tile-smem.f, tile-acc.f, tile-v4.f, tile-v4a.f, cg-vec.f, collective.f, and cg-collective.f expose active tile/register/shared-memory/collective helpers as raw global stems across roughly 200 definitions. The PTX package block in collective.f owns only the checked public collective DSL; most implementation state remains global. Reopen concern-specific PTX-TILE and PTX-COLLECTIVE packages: keep logical checked DSL operations public, keep tile allocation, loop labels, shared-memory cursors, accumulator state, vector lane helpers, and emission stencils private, and replace callers with qualified short tails without compatibility aliases. Preserve exact PTX bytes, register numbering, barrier/uniformity behavior, predicated tails, alignment obligations, shared-memory layout, resource counts, and numerical device results. Add negative fixtures proving every retired tile/vector/collective pseudo-global and qualified private helper rejects; public API positives cover scalar, v4, v4a, loop, shared-memory, accumulator, and collective paths. Measure dictionary-name bytes, loaded JIT/DATA, CODELEN, compile latency, and emitted instruction/resource counts; require no unexplained growth. Verify every focused positive/negative tile/collective suite, ptx-stdlib, Maki reductions/vectorization, device goldens, typed-local diff, package/host/filemap/dot lints, fixpoint, and full native gate. Parent: habu-pkg-ptx-compiler-db8cfa46; package boundary only, excluding the separate legacy-v4 semantic migration.
