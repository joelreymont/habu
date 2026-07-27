---
title: Prove compiler ID allocator
status: open
priority: 1
issue-type: task
created-at: "2026-07-27T14:39:02.884735+02:00"
blocks:
  - habu-model-compiler-id-9808ce6c
---

Scope: prove formal/Common/IdAllocatorLaws.v over the allocator transition/CAS model. Prove monotonic successful allocation, nonzero issued serials, uniqueness under all modeled interleavings, stale-CAS retry preservation, and exhaustion before increment or wrap. Acceptance: Rocq 9.2 proves every law with no Admitted; weakening the domain, success transition, retry rule, or exhaustion guard breaks a mutation proof; assumptions report exposes exactly the allocator model's atomic-CAS linearizability assumption. Ownership: formal/Common/IdAllocatorLaws.v and focused mutation proofs only. Excludes checked implementation, ID packing laws, parity runner, require replay, arenas, dialects, native/GPU, and maki. Depends on habu-model-compiler-id-9808ce6c.
