---
title: Add provisional KV row
status: active
priority: 1
issue-type: task
created-at: "2026-07-30T06:31:39.269165+02:00"
blocks:
  - habu-open-provisional-kv-291eed23
---

Why: a begun batch must stage one distinct sequence without publishing any committed sequence length. Result: ADD ( KV:cache KV:batch KV:seq -- KV:add-result ) validates a distinct live sequence, declared maximum, reservation, page and free-list capacity, copy-on-write reserve, layer-aware device destination, and arithmetic; add-result is exactly added(cache,batch) or refused(cache,batch,n), with no separate batch-error type. Committed lengths and page tables remain unchanged. Provisional page occupancy updates the existing monotonic HIGH-WATER exactly like ordinary allocation; cancellation restores pages, references, and reservations but never lowers HIGH-WATER. Private nseq-bounded row and duplicate state is sized by CONFIG-VALUES, stored in OPEN's sole host allocation, initialized by STORE-DIMS, and reused without any post-OPEN allocation; exact footprint assertions move with that formula, and the mmap-exhaustion child executes ADD. Owner: ADD and its provisional row state only. Dependency: open provisional KV batch. Production red: the current append path publishes one sequence independently. Acceptance: unique, boundary, and copy-on-write rows stage deterministically; duplicate, stale, over-maximum, missing-reservation, full-list, overflow, and wrong-layer rows refuse with cache and batch intact; success changes only provisional state plus the exact monotonic HIGH-WATER; no descriptor, cancellation, DEVRT, launch, or commit appears. Forbidden: single-row wrapper, public constructor, new metric, compatibility, or lint. Smallest owning check: real ADD success plus each refusal and exact HIGH-WATER transition through maki/infer/kv-cache-test.f. Claim: agent=codex-kv-add workspace=.jj-ws/habu-add-provisional-kv-b013791a.
