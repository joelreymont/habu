---
title: Validate Qwen tensor layouts
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:55:50.281335+02:00"
blocks:
  - habu-add-qwen-model-bf23d2ff
  - habu-parse-qwen-config-cd65019c
  - habu-infer-dense-tensor-c037a6fd
---

Why: Qwen dtype, shape, orientation, and bias rules are distinct from role naming and slot lookup. Interface: QWENTENSOR:LAYOUT takes validated MDLCFG plus role and returns the exact BF16 shape/orientation; Q/K/V bias is required and O/MLP bias forbidden. Owner: Qwen tensor layout authority only. Production red: device allocation cannot prove extents. Acceptance: first/middle/last layer roles and all non-layer roles match exact extents; wrong dtype, shape, orientation, bias, layer, and overflow reject. Forbidden: name parser, slot allocation, shard mapping, duplicate table, version, or compatibility arm. Smallest owning check: bin/hb --load maki/infer/qwen-tensor-layout-test.f.
