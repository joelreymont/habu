---
title: "CAD 2: fusion region discovery + traffic estimate"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T00:20:24.222944+02:00"
---

docs/model-cad.md Phase 2. Over cad-1 IR: straight-line elementwise region discovery, matmul/linear+bias+activation epilogue regions, legality constraints (materialization, layout, barrier/reduction boundary, backward rule, tolerance, pinning), profitability estimate (global bytes removed, launches removed, extra FLOPs). FUSE report: ops before/after, regions, materialization points, est bytes before/after, named split reason per split, risk flags. Codegen lever stays in habu-automatic-op-fusion / habu-automatic-aggressive-fusion; this dot is the planner+report. Depends: cad-1.
