---
title: "Own inference engine"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.395660+02:00"
blocks:
  - habu-own-gpt-2-22b5e92b
  - habu-infer-kv-atomic-c402952e
---

Why this exists:
the owned model and one multi-sequence paged cache need a single engine lifetime before request rows are added.

Required result:
package INFER defines one linear engine. START ( INFER:model n n -- INFER:start-result ) takes maximum live sequences and total KV tokens, validates both against the model config, opens one layer-aware device KV cache with that exact shared capacity through the model session, and publishes only after every acquisition succeeds. The active model arm supplies config, public name, batch capability, tokenizer bounds, and stop set through private closed dispatch. FOOTPRINT returns the immutable model, engine-host, and KV host/device bytes without double counting. STOP rejects while any sequence row or KV batch is live, then releases the cache and model. Sequence storage and handles are added by the next leaf. No engine-per-request wrapper, second cache, raw model handle, global buffer, pack, callback, version, or architecture-specific engine type.

Done when:
two engines coexist; zero and overflowing capacities reject; injected failure at every START acquisition returns the model; FOOTPRINT equals the stored allocation extents; STOP rejects with live sequence or batch state and otherwise returns every cache, host, device, and model owner.

Expected touch points: new maki/infer/engine.f and focused test.
Smallest check: bin/hb --load the focused engine start/stop test through the real model and device cache path.
Prerequisites: owned GPT-2 inference model and the sole KV lifetime.
Owned result: the sole engine and multi-sequence cache lifetime only.
Claim: unassigned.
