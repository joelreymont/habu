---
title: Close KV sequences atomically
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:38:16.941826+02:00"
blocks:
  - habu-finalize-provisional-kv-b8b46613
---

Why: closing several inference rows sequentially can return pages for early rows before a later invariant failure, preventing atomic scheduler retirement. Result: replace public CANCEL-SEQ with the sole KV:CLOSE-MANY ( KV:cache ptr KV:seq CAD-NUM:item-count -- KV:close-result ). It accepts one or more distinct opaque live sequences from the same cache only after no provisional batch contains them. One aggregate preflight resolves every identity and validates all page lists, shared-reference deltas, free-list capacity, token, page, and copy reservations, arithmetic, and final allocator invariants. Only then one total commit retires all handles, decrements aggregate references, returns newly unowned pages, and releases reservations; no fallible operation remains after the first store. It returns closed(cache) or refused(cache,seq,code) with no mutation, where seq is the first failing copyable handle. INFER is the only external caller and uses the same operation for one and many rows. Owner: whole-sequence KV reclamation only. Dependency: finalized provisional append transition. Production red: current single-row cancellation mutates reference state before its last invariant check. Acceptance: one, several, empty, partial-tail, full-page, forked, shared-page, and failed-prefill shapes reclaim exactly; duplicate, stale, cross-cache, pending-batch, overflow, and injected failure at every preflight position leave the whole cache unchanged; shared peers remain exact; CANCEL-SEQ is absent. Forbidden: scheduler mutation, partial close, retry state, snapshot, compatibility wrapper, metric, or lint. Smallest owning check: bin/hb --load maki/infer/kv-cache-test.f with every list shape and failure position. Claim: unassigned.
