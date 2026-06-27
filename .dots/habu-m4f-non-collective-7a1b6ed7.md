---
title: "M4f: non-collective negative regressions"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:11:55.494260+02:00"
blocks:
  - habu-m4d-elementwise-broadcast-475a8b91
---

Decomposes M4. Commit the non-collective negatives from ptx-sketch.md Acceptance #4 as gate regressions: wrong-space load, missing ctx, extent mismatch, mixed masks, raw pointer arithmetic on a span. The checker already rejects these (M2) - pin them so a refactor cannot lose them.
- Files: lib/ptx/tile-test.f (or tools/ptx-check-test.f, shared with the M2-negatives dot); wire into test/run.f.
- Verify: each negative REJECTS through the owning bin/hb --load path with the expected diagnostic substring.
- Dep: M4d (also pairs with the M2 parametric-negatives dot).
