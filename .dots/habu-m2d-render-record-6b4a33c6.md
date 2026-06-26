---
title: "M2d: render/record round-trip for parametric terms"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:03:30.810071+02:00"
blocks:
  - habu-m2b-t-param-02ccd510
---

Part of PTX M2. Render a parametric term to its exact source spelling and record/restore it across the checker store + snapshot (ptx-sketch.md M2); needed so signatures survive warm images (LESSONS.md).
- Files: src/core/render.f + checker store serialization.
- Verify: parse then render equals input for span/matrix/tile/gridctx/rowctx/uniform/rowidx; record then restore preserves fields.
- Dep: M2b (parallel to M2c).
