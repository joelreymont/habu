---
title: Own GPT-2 runtime storage
status: closed
priority: 2
issue-type: task
created-at: "2026-07-30T00:55:48.437756+02:00"
closed-at: "2026-08-04T19:31:48.048346+02:00"
close-reason: "Superseded by GPT2:model at 57a19153daab and 1056a0ad14d5: checked config-derived layout, one GPU:buffer for weights/activations/logits/token/K/V, persistent addresses, allocation cleanup, and close are already owned; DEVRT storage would duplicate them."
---

Why: activation, descriptor, logit, and workspace extents must derive once from validated GPT-2 config. Interface: package-private DEVRT:PLAN-GPT2 computes checked host/device extents and block plan; ALLOC-GPT2 takes core plus plan and returns private storage, while RELEASE consumes it. Owner: GPT-2 runtime storage and immutable footprint query only. Production red: launch.f has fixed 4096-element buffers. Acceptance: exact GPT-2 geometry and one-over/overflow cases, allocation fault matrix, stable addresses, and FOOTPRINT byte sums pass. Forbidden: module loading, kernel code, per-token allocation, fixed cap, model pack, version, or compatibility path. Smallest owning check: bin/hb --load maki/infer/gpt2-runtime-storage-test.f on DGX Spark.
