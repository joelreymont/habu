---
title: "Infer KV: atomic cancellation cleanup"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:38:16.941826+02:00"
blocks:
  - habu-finalize-provisional-kv-b8b46613
---

Why this exists:
cancellation and failed-prefill cleanup return page and reservation owners; discovering a defect after the first return can strand or double-return state.

Required result:
preflight the whole sequence row, opaque identity, page list, per-page reference counts, free-list capacity, unused token/page/copy reservations, and absence from the pending append batch, then atomically retire the handle, decrement shared references, and return each newly unowned page and reservation. Empty, partial-tail, full-page, forked/shared, and failed-prefill sequences use this single transition. INFER calls it only at a type-proven no-pending boundary after RUN-ROWS finalized any provisional batch. Do not add scheduler mutation, snapshot pins, retryable-unmap state, log-and-continue cleanup, or a second cancellation policy.

Done when:
injected invariant failures mutate nothing; every ordinary and shared sequence shape returns exact ownership without changing survivors; a sequence in the pending batch rejects until that batch commits or aborts; stale and double cancellation reject; focused KV tests pass.

Expected touch points: maki/infer/kv-cache.f, maki/infer/kv-cache-test.f.
Smallest check: bin/hb --load maki/infer/kv-cache-test.f.
Prerequisites: finalized provisional append transition.
Owned result: cancellation and failed-prefill cleanup only.
Claim: unassigned.
