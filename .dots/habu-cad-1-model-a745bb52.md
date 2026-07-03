---
title: "CAD 1: model IR node table + shape/layout facts"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T00:20:24.217245+02:00"
---

docs/model-cad.md Phase 1. maki/model-ir.f + test. Index-based node table (recursive ADTs blocked until TFAM 16 boxed; use typed idx into table, lib/ptx/ir.f pattern): op kind, operand indices, shape, dtype, layout, attrs, materialization requirement, autograd metadata. Shape/layout keys; region extraction for fusion candidates; fail-closed unsupported ops (maki/onnx.f pattern); serializable for agent logs + artifact cache. Op set v1: add mul scale bias relu gelu gelu-approx layernorm rmsnorm softmax-row matmul linear residual-add cast. Depends: cad-0a.
