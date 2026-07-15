---
title: Migrate memory numeric allocation owner
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T14:14:22.894092+02:00"
blocks:
  - habu-seal-cad-num-36dbeec6
---

Full context: lib/memory.f accepts raw n/count and enforces positivity at runtime, while B5 requires package-first typed allocation roles. Fix: create/reopen public MEM and add exactly MEM:CELLS>BYTES, MEM:64K-BYTES, MEM:64K-COUNT-FOR, MEM:64K-SPAN-BYTES, MEM:ALLOC-BYTES, MEM:ALLOC-CELLS, and MEM:ALLOC-64K with the effects frozen in MODEL-CAD-V2-PLAN.md B5.5; keep zero-admitting roles out of allocation sinks. Add only two MEM-private audited TRUSTED projections ALLOC-BYTES>N at mmap and ALLOC-CELLS>N before cells, with no public export and primitive-typing removal condition. Do not remove or tighten legacy MEM-ALLOC-BYTES until all four caller waves land; MEM-ALLOC-CELLS and multi-64K legacy conveniences are outside this wave. Acceptance: positive signature control, byte/cell role swaps and zero allocation reject, first overflow/over-allocation fails before mmap, exact legacy behavior for positive values. Files: lib/memory.f, lib/memory-test.f, TRUSTED.md. Verify exact tests, refine/trust, native memory slices. Depends on sealed CAD-NUM and arithmetic.
