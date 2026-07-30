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
package INFER defines one linear engine and one immutable copyable INFER:info value. START ( INFER:model n n -- INFER:start-result ) takes maximum live sequences and total KV tokens, validates both against the model config, opens one layer-aware device KV cache with that exact shared capacity through the model session, and publishes only after every acquisition succeeds. The active model arm supplies config, public name, batch capability, tokenizer bounds, and stop set through private closed dispatch. INFO ( INFER:engine -- INFER:engine INFER:info ) is the sole public projection of the static model name span, valid token count, maximum bytes emitted by one token, and model batch cap; those fields are copied from the closed model dispatch and have no second authority. FOOTPRINT returns the immutable model, engine-host, and KV host/device bytes without double counting. STOP rejects while any sequence row or KV batch is live, then releases the cache and model. Sequence storage and handles are added by the next leaf. No engine-per-request wrapper, second cache, raw model handle, global buffer, pack, callback, version, or architecture-specific engine type.

Done when:
two engines coexist; their INFO values select the exact model name, valid count, maximum token bytes, and batch cap without exposing a model arm; zero and overflowing capacities reject; injected failure at every START acquisition returns the model; FOOTPRINT equals the stored allocation extents; STOP rejects with live sequence or batch state and otherwise returns every cache, host, device, and model owner.

Expected touch points: new maki/infer/engine.f and focused test.
Smallest check: bin/hb --load the focused engine start/stop test through the real model and device cache path.
Prerequisites: owned GPT-2 inference model and the sole KV lifetime.
Owned result: the sole engine and multi-sequence cache lifetime only.
Claim: unassigned.
