---
title: Stage one Qwen shard
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T22:01:12.625329+02:00"
blocks:
  - habu-validate-qwen-shard-2aa7d74f
  - habu-own-qwen-device-f2a8083c
  - habu-use-canonical-checkpoint-92eac785
  - habu-validate-qwen-tensor-0fba9ad6
  - habu-own-model-asset-c6f938e4
---

Why: the four large shard files must be authenticated and copied without a full host model. Result: package-private DEVRT:STAGE-QWEN-SHARD takes qbuild, private DEVRT:qstage, MODEL-ASSET:ws, MDLCFG:mcfg, QWENIDX:index, a root ptr u8 plus CAD-NUM:byte-len, and one shard ordinal. Its exact result is staged(qbuild,qstage,index,ws) or refused(qbuild,qstage,index,ws,stage-error), where stage-error is path, open, authentication, parse, catalog, copy, synchronization, or close. It authenticates the exact QWENPIN shard once, validates each assigned SAFET tensor name, canonical BF16 dtype, shape, extent, and orientation, and calls PUT-QWEN-WEIGHT for each role while copying through the workspace buffer. SHA-256 covers the same bytes consumed by validation and staging; every copy completes before buffer reuse; the shard closes before staged returns. QWENTENSOR and QWENIDX are the only catalog and tensor-role authorities. Owner: one private DEVRT shard stage only. Production red: no path can authenticate and copy a shard through the qbuild-owned weight transition. Acceptance: each real shard stages its exact assigned roles; every named failure returns all outer owners with no published WEIGHTS slot; instrumentation proves one open and hash. Forbidden: package-global path or staging buffer, public stage, WSTORE, QWENLOAD model, full-shard buffer, two open shards, second catalog, filename inference, lazy load, retry state, pack, version, or compatibility reader. Smallest owning check: bin/hb --load maki/infer/qwen-device-stage-test.f on DGX Spark. Claim: unassigned.
