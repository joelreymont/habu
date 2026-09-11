---
title: Prepare inference rows
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:01:46.965139+02:00"
blocks:
  - habu-own-inference-sequence-990a6ff3
---

Why: row authentication and capacity checks are a separate total phase from the device and KV transaction. Result: package-private INFER:PLAN-ROWS takes an engine, ptr INFER:input-row, and CAD-NUM:item-count; it validates a nonempty set of distinct live sequences, next-token bounds, context and reservation capacity, engine descriptor and logit storage, and the active model batch cap, then copies immutable row facts into engine-owned plan storage and returns a private linear plan. It changes no sequence, KV, random, logit, output, or device state. DISCARD-PLAN consumes a plan without mutation. RUN-ROWS is the only plan consumer. Dependency: inference sequence rows. Owner: row preflight and immutable plan only. Production red: RUN-ROWS currently mixes caller validation with the device transaction. Acceptance: one, two, and model-cap rows plan deterministically; empty, duplicate, stale, cross-engine, invalid token, context, reservation, short storage, and over-cap inputs reject before mutation; input edits after return cannot change the plan; discard restores the plan slot. Forbidden: KV batch, launch, sampling, sorting, allocation, scheduler state, public plan fields, version, or compatibility path. Smallest owning check: bin/hb --load maki/infer/engine-plan-test.f.
