---
title: Prove compiler ID schema
status: open
priority: 1
issue-type: task
created-at: "2026-07-27T13:22:24.314793+02:00"
blocks:
  - habu-add-compiler-ir-21e976fc
---

Full context: coordinate the stage-local proof of IR-0.1 only: freeze its checked canonical ID manifest and reachable numeric vectors, model exact 64-bit identities and predicates in Rocq, prove bounded packing laws, separately model and prove the process-wide CAS allocator, then bind digest, numeric parity, static wrong-family rejection, require replay, and the explicit atomic-CAS assumption. Children own all implementation. Dependency: habu-add-compiler-ir-21e976fc. Acceptance: all six bounded leaves land; Rocq 9.2 builds with no Admitted; the canonical digest, reachable valid/hostile decisions, static schema distinction, allocator laws, and replay fixture agree; the assumptions report exposes atomic-CAS linearizability. Excludes shared records, tables, opcodes, general witnesses, dialects, native/GPU theorems, and maki/infer.
