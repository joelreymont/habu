---
title: "Infer engine: owned single-sequence state"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.395660+02:00"
blocks:
  - habu-infer-pack-boring-c8e07d29
  - habu-infer-kv-retryable-b548fcd2
---

Why this exists:
loader, model, KV sequence, workspaces, sampler state, and tokenizer state need one explicit lifetime before they are composed.

Required result:
define an owned single-sequence engine state whose initialization publishes only after every dependency succeeds and whose disposal preserves retryable owners.

Done when:
injected failure at every acquisition leaves no published engine and releases prior owners exactly once; stale and double disposal reject.

Expected touch points: new maki/infer/engine-state.f, focused test, FILEMAP.md.
Smallest check: focused engine-state test plus mmap/munmap trace.
Prerequisites: model-pack runtime loader and paged KV retryable disposal.
Owned result: engine state acquisition and lifetime only.
Claim: unassigned.
