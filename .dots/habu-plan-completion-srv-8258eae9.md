---
title: Plan completion server storage
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:31:04.810083+02:00"
blocks:
  - habu-own-completion-srv-2cffaf4b
  - habu-infer-engine-owned-99a98d17
  - habu-infer-scheduler-req-1ac1dac6
  - habu-frame-bounded-http-d677ca95
  - habu-frame-completion-http-b2039e63
  - habu-render-completion-json-9fff2d34
---

Why: model-dependent response and scheduler storage can be sized only after the engine and scheduler publish their canonical limits. Result: package SERVE defines immutable plan and PLAN ( INFER:info SCHED:result-cap SERVE:conn-cap SERVE:body-cap SERVE:prompt-cap SERVE:output-cap -- SERVE:plan-result ). It uses HTTP-COMP request and response bounds, JSON-WRITE:STORAGE-BYTES, OPENAI-COMP:RESPONSE-BOUND, and SCHED result capacity to derive every per-connection span, poll rows, the sole result table and arena, alignment, and exact total bytes with checked arithmetic. It allocates nothing and is the sole authority consumed by SERVE:OPEN and FOOTPRINT. Owner: server storage planning only. Production red: typed caller options cannot be turned into one exact server allocation without duplicating model limits. Acceptance: both model info values, every wrong-role PLAN permutation, every decimal and escaping edge, exact, one-over, alignment, product, sum, and total overflow produce exact plan, static rejection, or refusal as appropriate. Forbidden: argument parsing, requested-limit constructor, model selection, allocation, listener, OPEN, duplicated model field, generic limit type, default, version, compatibility, metric, or lint. Smallest owning check: bin/hb --load maki/serve/server-plan-test.f. Claim: unassigned.
