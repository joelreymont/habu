---
title: Replace fixed inlining threshold with cost model
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T19:49:06.366217+02:00"
blocks:
  - habu-aot-repl-bl-a71440da
---

Measured at master 3909bbac. src/habu/habu2.f sets INL-MAX to 40 bytes and C-CALL copies every safe eligible body without considering call cost, call count, total code growth, or measured hotness. DUPH ( n -- n n ) dup is 44 bytes with a 24-byte copied body: one caller is 44 bytes and two calls are 68 bytes, while current absolute calls would be 36 and 52 bytes, so the inliner adds 8 bytes per call today; after direct BL, copying that body costs 20 bytes more than a 4-byte call. A four-call INC fixture is 100 bytes versus 36 bytes with direct BL, 64 bytes of avoidable duplication. Root cause: safety eligibility is being used as an optimization policy. Fix after habu-aot-repl-bl-a71440da: use a deterministic size cost model with direct BL as the default; inline only when size-neutral, explicitly required by a measured hot path, or proven by PERF-VERDICT to repay bounded growth. Keep branch/ADR/return-slot safety scans as correctness checks, separate from the policy. Acceptance: exact one-, two-, and four-call sizes are pinned; repeated helper use has a code-growth budget; tree-shaken and AOT builds use the same policy; approved hot expansions carry benchmark evidence and unchanged semantics; runtime parity, AOT closure/relocation, snapshot, bootstrap mirror, native fixpoint x2, both targets, full gates, and exact size ratchets pass. Files: src/habu/habu2.f, bootstrap/cg/forth.fs, JIT disassembly/size tests, PERF-VERDICT fixtures, and size gates.
