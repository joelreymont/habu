---
title: "M2b: T-PARAM side-table parametric terms"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:03:30.769750+02:00"
blocks:
  - habu-m2a-signature-token-9168056e
---

Part of PTX M2. Represent parametric terms span<space-global,f32,extent-n>, matrix<S,T,R,C>, tile<T,B,M> as T-PARAM side-table entries (ptx-sketch.md M2): head constructor plus ordered field terms.
- Files: src/core/checker.f term representation; keep render/record in sync (render.f).
- Verify: construct/inspect a span term; a 3-field tile stores 3 fields.
- Dep: M2a.
