---
title: Name Qwen tensors
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:55:50.152292+02:00"
blocks:
  - habu-parse-qwen-config-cd65019c
  - habu-infer-dense-tensor-c037a6fd
---

Why: 339 Qwen tensor roles need one exact name render/parse bijection independent from layout validation. Interface: package QWENTENSOR renders each role/layer to its pinned checkpoint name and parses only those names back, using caller storage and validated layer bounds. Owner: Qwen tensor naming only. Production red: no name authority can drive shard-index validation. Acceptance: all 339 names round-trip; wrong layer, bias, prefix, suffix, duplicate, and short storage reject; hostile comments/strings cannot satisfy tests. Forbidden: dtype, shape, slot table, filename map, generated artifact, version, or compatibility alias. Smallest owning check: bin/hb --load maki/infer/qwen-tensor-name-test.f.
