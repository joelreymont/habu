---
title: Model compiler ID allocator
status: open
priority: 1
issue-type: task
created-at: "2026-07-27T14:39:02.868347+02:00"
blocks:
  - habu-add-compiler-ir-21e976fc
---

Scope: define formal/Common/IdAllocator.v for the IR-0.1 process-wide module-serial allocator. Model the aligned atomic state, successful and failed CAS transitions, interleavings, and the 1..0x7fffffff serial domain. Record atomic-CAS linearizability as one explicit external assumption unless an existing primitive proof is bound. Acceptance: Rocq 9.2 builds; transitions are executable; zero, out-of-domain, stale-CAS, and exhausted states decide correctly; no allocator laws beyond definitional examples; no Admitted. Ownership: formal/Common/IdAllocator.v only. Excludes checked implementation, ID packing laws, allocator theorems, parity, replay, arenas, dialects, native/GPU, and maki. Depends on habu-add-compiler-ir-21e976fc.
