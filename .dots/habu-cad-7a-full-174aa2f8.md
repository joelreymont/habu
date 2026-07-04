---
title: "CAD 7a: full-tensor host executor"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T08:00:06.749353+02:00"
---

Pulled forward from cad-7 (blocks real MLP gradcheck AND habu-maki-from-scratch training). Execute the model IR at TENSOR granularity on host buffers: walk nodes in topo (node) order, allocate per-node host float-cell buffers from declared shapes, dispatch each op-kind to its buffer-level reference - matmul/linear (new buffer reference words needed: 2D matmul over float cells + linear with bias; these also complete the matmul/linear/cast registry rows), rowsum/fullsum/pad-scatter/scatter-add (exist: reduce-bwd.f/scatter.f), movement (move.f), norms/softmax rows (layernorm.f/rmsnorm.f/softmax.f row loops), elementwise (scalar refs mapped over buffers), rope pairs. Then: (1) gradcheck.f extends GC-APPLY to this executor - MLP gradchecks for real (analytic vs FD at buffer granularity, per-element compare); (2) the executor IS cad-7's GOLDEN composition oracle; (3) the training flagship uses it for forward+backward steps. Checked, fail-closed on unexecutable ops (only cast + decode remain), tests with hand-computed matmul values + MLP end-to-end gradcheck PASS. Depends: cad-9e (landed). Blocks: habu-maki-from-scratch, cad-7-optimize GOLDEN.
