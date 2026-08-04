---
title: Open provisional KV batch
status: active
priority: 1
issue-type: task
created-at: "2026-07-30T06:31:31.208766+02:00"
blocks:
  - habu-infer-kv-declared-a0319bef
  - habu-own-device-kv-8e5bbf98
---

Why: a cache needs one linear carrier before rows can be staged without changing committed sequence lengths. Result: package KV declares public `DEFLINEAR KV:batch`. Its sole representation is one globally unique nonzero batch generation; a private trusted mint/take pair follows the existing `KV:cache` boundary and cites the same retirement owner. The cache header stores the one active generation. The global counter refuses with `E-KV-ID` at exhaustion and never wraps or reuses a value. `BEGIN-BATCH ( KV:cache -- KV:cache result<KV:batch,n> )` refuses with `E-KV-BATCH` while a batch is active; otherwise it commits the next generation and returns the sole batch owner. `cancel-result = cancelled(KV:cache) | refused(KV:cache,KV:batch,n)`. `CANCEL-BATCH ( KV:cache KV:batch -- KV:cancel-result )` consumes the matching zero-row batch and clears the active generation, or returns both owners unchanged with `E-KV-BATCH` for a stale or cross-cache batch. Owner: the exact batch lifetime, generation state, BEGIN-BATCH, and zero-row CANCEL-BATCH only. Dependencies: declared admission, opaque sequence identity, device KV ownership, and layer-aware KV storage. Production red: no provisional owner can be opened and honestly eliminated through the real KV path. Acceptance: begin succeeds once; a concurrent begin refuses without mutation; matching cancel clears exclusivity; the next begin uses a strictly newer generation; stale and cross-cache cancellation return both owners unchanged; exhaustion refuses before mutation; batch duplication, drop, raw construction, and foreign mint/take use reject before lowering. No committed length, page, reservation, HIGH-WATER, device storage, or descriptor state changes. Forbidden: ADD, ready or descriptor type, provisional row or descriptor storage, DEVRT type, launch, commit, special begin result or batch-error type, compatibility, metric, lint, or test-only eliminator. Smallest owning check: real BEGIN-BATCH/CANCEL-BATCH lifetime, duplicate-begin, generation, cross-cache, exhaustion, and linear negative paths through maki/infer/kv-cache-test.f. Claim: agent=codex-kv-batch-open workspace=.jj-ws/habu-open-provisional-kv-291eed23
