---
title: "Equation op-kind: einsum ops in the trainable graph"
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T11:05:59.528203+02:00"
---

Stage 1 of docs/model-unified.md (ratified MODEL:-unification design). Add ONE op-kind  whose attrs cell carries the equation registry slot (the model-ir.f:116 attrs mechanism segment-attention uses); registration at declaration time maps name -> (kernel xt, dataflow record, extents); executor forward arm dispatches to the generated host word; composition-time extent-unification check replaces nothing yet (old SHP-LEGAL? stays for old ops). Acceptance: attention scores S[q k] = Q[q c] K[k c] * +SUM c declared as an equation runs INSIDE a composition and matches maki/attention.f MM-NT numerically.
