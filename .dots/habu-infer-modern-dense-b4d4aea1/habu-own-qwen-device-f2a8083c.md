---
title: Own Qwen runtime weight slots
status: open
priority: 1
issue-type: task
created-at: "2026-07-29T23:22:19.976146+02:00"
blocks:
  - habu-infer-dense-tensor-c037a6fd
  - habu-validate-qwen-tensor-0fba9ad6
  - habu-open-qwen-device-0db2dea3
---

Why: the opaque qbuild needs one exact internal transaction for its preallocated weight region and descriptor rows. Result: package DEVRT defines private qstage and exact BEGIN-QWEN-WEIGHTS, PUT-QWEN-WEIGHT, FINISH-QWEN-WEIGHTS, and CANCEL-QWEN-WEIGHTS transitions. BEGIN consumes and returns qbuild plus an empty qstage; PUT accepts one QWENTENSOR role and checked destination extent, fills it once, and returns both owners; FINISH succeeds only after all 339 roles are present and returns qbuild with WEIGHTS set; CANCEL consumes any qstage and returns qbuild with every descriptor empty. Private lookup through qbuild or the completed Qwen session accepts only a QWENTENSOR role and never returns a raw pointer. No checkpoint bytes are read here. Owner: sole qbuild weight-staging state, descriptor rows, exact mutation transitions, lookup, and weight footprint only. Production red: QWENTENSOR roles have no legal DEVRT-owned route into opaque qbuild storage. Acceptance: every catalog role maps to one non-overlapping in-range descriptor; duplicate, missing, wrong role, overflow, partial fill, stale generation, finish, and cancellation failures follow exact owners; cancellation restores the empty inventory; two builders coexist. Forbidden: QWENDEV weights owner, public stage, public descriptor, second allocation, per-tensor allocation, public pointer, duplicated catalog, loader, conversion, generic storage transition, plugin, version, or compatibility format. Smallest owning check: bin/hb --load maki/infer/qwen-runtime-weights-test.f on DGX Spark.
