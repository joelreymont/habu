---
title: Design typed mutable span for compute
status: open
priority: 1
issue-type: task
created-at: "2026-07-28T20:03:00.937387+02:00"
---

Why: three lanes converged today on the same missing checker capability, and one produced a rejected commit. The public raw ptr-plus-signed-count compute shape was destruction-rejected twice for GELU! (codex-70ff this morning; commit 1122785b5bfe this evening, where a probe proved n=-1 executes and mutates cell zero and an oversized count has no capacity witness); MM-NT's six same-typed extents let a caller transpose dimensions and still certify; and the T-ADD!/ROW-ADD pair proved identical signatures with opposite argument roles certify interchangeably. Exact result: the frozen design - not implementation - of one typed mutable span value carrying pointer and extent with a capacity role, package-owned indexed access, no caller-supplied length trusted; its owner (likely package MAKI beside array.f); the migration order for GELU, the published MM-NT, EMBED-ROWS, ADD!, and the LayerNorm wrappers onto it; and the adjacent write-effect checker gap (fail-closed-before-write) recorded as its own capability entry. Every new public compute mutator waits on this design; consolidation moves of existing surface (the ADD! leaf) proceed because they add no new unsafe API. Owner: design document reviewed jointly by both orchestrators before any implementation dot is minted. Dependencies: none. Acceptance: both orchestrators accept the frozen type, owner, and migration order; each migration step is a named sub-30-minute leaf; the design names what the checker enforces at each step and what remains runtime-thrown. Forbidden: implementation in this leaf, raw-shape compromises, per-word local guards presented as the fix.
