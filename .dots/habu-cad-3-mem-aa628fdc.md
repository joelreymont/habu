---
title: "CAD 3: memory/coalescing report"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T00:20:35.485430+02:00"
---

docs/model-cad.md Phase 3. MEMORY command: per-hot-tensor plan (layout, stride, alignment, contiguity, vector width, lane mapping, address space, broadcast, tail masks, smem bank behavior) + traffic before/after fusion. v4 selection when alignment/shape legal, warned fallback otherwise; deliberately strided access reports non-coalesced. Facts source: lib/ptx/tile-v4.f, lib/ptx/tile-smem.f. Depends: cad-1.
