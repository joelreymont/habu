---
title: "Infer engine: 64-token GPT2 oracle"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.417979+02:00"
blocks:
  - habu-infer-engine-sample-0f2a4ef4
  - habu-infer-gpt2-greedy-b456d5b6
---

Why this exists:
M4 requires a complete prompt-to-token proof, not isolated stages.

Required result:
compose engine initialization, prefill, paged decode, greedy selection, and detokenization for the fixed GPT-2 prompts.

Done when:
at least 64 token identifiers match the trusted reference exactly, run twice is identical, every selected internal checkpoint is inspectable, and all owners return after completion and cancellation.

Expected touch points: end-to-end engine test and canonical fixture.
Smallest check: correctness-only GB10 64-token run and cleanup trace.
Prerequisites: sample and detokenize step and GPT-2 greedy oracle loop.
Owned result: M4 end-to-end correctness acceptance only.
Claim: unassigned.
