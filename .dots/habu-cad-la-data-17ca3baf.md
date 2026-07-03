---
title: "CAD LA: data-movement ops as IR facts"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T00:37:53.001869+02:00"
---

Driving workload demand (LocateAnything port kernel order, item 1; docs/model-cad.md Phase 1). Add reshape/view, transpose, slice, concat, gather to the cad model IR as layout-transform FACTS the planner reasons about (stride/layout rewrites), not eager kernels; materialize a copy kernel only where a consumer genuinely requires contiguous layout, and report that materialization in the MEMORY plan. Extend maki/onnx.f lowering for these ops fail-closed->supported. Depends: cad-1-ir. Related: habu-maki-onnx-graph.
