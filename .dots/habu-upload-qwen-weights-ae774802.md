---
title: Load Qwen runtime weights
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T22:02:17.361851+02:00"
blocks:
  - habu-load-qwen-weights-656bdf44
  - habu-open-qwen-device-0db2dea3
  - habu-own-model-asset-c6f938e4
---

Why: four private shard stages need one all-or-nothing qbuild inventory transition. Interface: DEVRT:LOAD-QWEN-WEIGHTS ( DEVRT:qbuild MODEL-ASSET:ws ptr u8 CAD-NUM:byte-len MDLCFG:mcfg -- DEVRT:qwen-load-result ) returns exactly loaded(qbuild,ws,high-water) or refused(qbuild,ws,load-error), where load-error is one closed enum: path, index, shard, open, authentication, parse, catalog, copy, synchronization, completeness, publication, or close. It opens the exact QWENIDX index through the workspace, calls BEGIN-QWEN-WEIGHTS, invokes STAGE-QWEN-SHARD exactly once for ordinals one through four, calls FINISH-QWEN-WEIGHTS only after all 339 descriptors are filled once, synchronizes, and releases the index and every SAFET source. Any failure calls CANCEL-QWEN-WEIGHTS and returns qbuild with WEIGHTS empty plus the workspace. Owner: complete Qwen weight-load transaction and sole WEIGHTS-slot publication point only. Production red: no call turns the pinned four shards into the qbuild's authenticated weight inventory. Acceptance: all 339 descriptors and selected values match the pinned checkpoint; zero SAFET, QWENIDX, WSTORE, qstage, and host-weight owners remain; every named failure returns qbuild with WEIGHTS empty and workspace; high-water equals the measured transient host peak; two loads coexist. Forbidden: QWENDEV weights product, public provisional state, second CUDA scope, implicit session, host model, F32 conversion, pack, lazy upload, raw pointer, per-layer allocation, duplicated catalog, version, or compatibility path. Smallest owning check: bin/hb --load maki/infer/qwen-device-load-test.f on DGX Spark. Claim: unassigned.
