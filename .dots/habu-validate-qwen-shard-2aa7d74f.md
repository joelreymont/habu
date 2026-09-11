---
title: Validate Qwen shard index
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T22:00:48.460433+02:00"
blocks:
  - habu-infer-dense-tensor-c037a6fd
  - habu-name-qwen-tensors-80bb3348
  - habu-infer-dense-pin-36c8e45c
  - habu-own-model-asset-c6f938e4
---

Why: four safetensors shards need one name-to-file authority. Interface: QWENIDX:OPEN takes and returns MODEL-ASSET:ws with a root ptr u8 plus CAD-NUM:byte-len, authenticates the QWENPIN index once, and publishes a linear index only after weight_map contains each of 339 QWENTENSOR names once, maps only to the four pinned basenames, names every shard, and contains no extra tensor or filename. LOOKUP returns the fixed shard ordinal and RELEASE consumes the index; every refusal returns the workspace. Reject unsafe or overlong root, duplicate JSON keys or mappings, missing or extra roles, path components, unknown or unused shards, overflow, wrong metadata total, and digest mismatch. The index is consumed inside DEVRT:LOAD-QWEN-WEIGHTS and never enters the model. Owner: new maki/infer/qwen-index.f only. Production red: no production owner validates the four-shard mapping. Acceptance: the pinned root maps exactly 339 roles; hostile or mutated files fail before device allocation with workspace ownership intact; one open and hash occur; two indices coexist and release totally. Forbidden: package-global path or file buffer, generic manifest, verified-root value, filename fallback, directory scan, pack, schema, or second tensor catalog. Smallest owning check: bin/hb --load maki/infer/qwen-index-test.f. Claim: unassigned.
