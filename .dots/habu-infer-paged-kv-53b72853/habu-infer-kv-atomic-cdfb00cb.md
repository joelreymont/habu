---
title: Build provisional KV batch
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-22T09:38:16.930498+02:00\""
blocks:
  - habu-infer-kv-declared-a0319bef
  - habu-add-kv-layer-41961bed
  - habu-use-opaque-kv-46d39625
  - habu-own-device-kv-8e5bbf98
---

Why: one device launch advances several sequences together; publishing any row before device completion would split ownership. Result: replace APPEND-TOKEN with the sole cache-owned provisional transaction. Package KV declares batch and ready as public linear owner products with no public constructors; foreign packages may only retain or pass them intact. BEGIN-BATCH ( KV:cache -- KV:begin-result ) returns begun(cache,batch) or refused(cache,batch-error). ADD ( KV:cache KV:batch KV:seq -- KV:add-result ) validates one distinct live sequence, declared maximum, reservation, page/free-list capacity, copy-on-write reserve, layer-aware device destination, and arithmetic, returning added(cache,batch) or refused(cache,batch,batch-error). DESCRIBE-BATCH ( KV:cache KV:batch ptr KV:desc CAD-NUM:item-count -- KV:describe-result ) copies immutable provisional lengths, device base and extent, layer layout, and block-table rows into the supplied bounded storage, then returns described(cache,ready,count) or refused(cache,batch,describe-error). ready retains only the cache's authenticated DEVRT session generation; descriptor rows and ready are separate. CANCEL-BATCH and CANCEL-READY return cancelled(cache) or refused(cache,the-input-carrier,cancel-error), restoring every provisional page, reference, reservation, and row only on success. Model and DEVRT receive only descriptor rows. Exactly one batch may be provisional per cache. Delete APPEND-TOKEN and every single-row wrapper.

This leaf contains no DEVRT type, launch, pending, DONE, QUIESCED, commit, post-enqueue cancel, or terminal batch identifier. The separate finalizer owns all post-enqueue publication and rollback. Add no public constructor, carrier field authority, snapshot, lease, boolean sync flag, compatibility API, metric, partial commit, or second policy. Owner: sole batch and ready declarations, provisional append construction, immutable descriptor export, and pre-enqueue cancellation only. Production red: current append authority can publish one sequence independently and has no legal cross-package transaction carrier. Acceptance: foreign construction of batch and ready rejects before lowering; injected failure at every BEGIN, ADD, DESCRIBE, CANCEL-BATCH, and CANCEL-READY point follows the exact result arms and leaves committed state byte-identical; unique, boundary, and copy-on-write batches describe deterministically; duplicate, stale, concurrent, over-maximum, missing-reservation, full-list, overflow, short-storage, wrong-layer, and empty batches reject with the owner intact. Smallest owning check: bin/hb --load maki/infer/kv-cache-test.f. Claim: unassigned.
