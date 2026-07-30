---
title: Start completion runtime
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:31:05.414453+02:00"
blocks:
  - habu-open-selected-inference-33d84548
  - habu-close-completion-runtime-7f8d56df
---

Why: the selected model must become one engine and scheduler before model-dependent capacities are trusted. Result: SERVE-CMD:START-RUNTIME consumes opts and INFER:model, starts INFER with max requests and KV tokens, obtains canonical INFER:info, validates the selected model name and requested batch against info.batch-cap, then starts SCHED with max requests and max batch. Success returns started(opts,scheduler,info). Every refusal invokes the cleanup helper for the exact latest published stage and returns its result unchanged. Owner: engine and scheduler startup plus canonical info validation only. Production red: model options cannot produce one scheduler without copied model constants. Acceptance: GPT-2 batch greater than one, Qwen batch one, excessive batch, zero or overflowing capacities, each acquisition failure, and each cleanup result preserve the exact remaining owner. Forbidden: cleanup implementation, server storage, listener, loop, request, duplicated model name or limits, per-request engine, version, compatibility, metric, or lint. Smallest owning check: bin/hb --load tools/serve-runtime-test.f on DGX Spark. Claim: unassigned.
