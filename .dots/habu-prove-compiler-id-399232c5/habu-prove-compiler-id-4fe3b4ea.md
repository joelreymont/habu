---
title: Prove compiler ID allocator
status: open
priority: 1
issue-type: task
created-at: "2026-07-27T14:39:02.884735+02:00"
blocks:
  - habu-model-compiler-id-9808ce6c
---

Scope: prove formal/Common/IdAllocatorLaws.v over the allocator transition/CAS model. Prove monotonic successful allocation, nonzero issued serials, uniqueness under all modeled interleavings, stale-CAS retry preservation, and exhaustion before increment or wrap. Emit theorem-local assumption evidence that exposes exactly the allocator model's named host-CAS operation and atomic-CAS linearizability law; this evidence stays with the focused leaf and is not the committed final report. Acceptance: Rocq 9.2 proves every law with no Admitted; weakening the domain, success transition, retry rule, or exhaustion guard breaks a mutation proof; local assumption evidence has no unexpected dependency. Ownership: formal/Common/IdAllocatorLaws.v, focused mutation proofs, and theorem-local generated assumption evidence only. Excludes checked implementation, ID packing laws, the committed expected-external-axiom manifest, the final committed assumptions report, parity runner, require replay, arenas, dialects, native/GPU, and maki. Depends on habu-model-compiler-id-9808ce6c.

Checkpoint:

1. Owner: Rocq module `Habu.Common.IdAllocatorLaws`; model dependency `Habu.Common.IdAllocator`.
2. Entry: `formal/Common/IdAllocatorLaws.v`.
3. Green: clean compilation of `Ids.v` and `IdAllocator.v`.
4. Red: extending that compile with `IdAllocatorLaws.v` fails before the theorem file and theorem-local `Print Assumptions` evidence exist.
5. Interface: monotonicity, nonzero, uniqueness, stale-retry, exhaustion-before-wrap, and host-refinement theorems, mutation counterexamples, and local assumption output.
6. Forbidden: stronger CAS axioms, hidden or renamed assumptions, weakened interleavings, post-wrap rejection, a committed report, parity work, Habu edits, or Admitted.
7. Focused: `rocq compile -Q formal Habu formal/Common/Ids.v formal/Common/IdAllocator.v formal/Common/IdAllocatorLaws.v`, checking local assumptions output.
8. Broader: every current `formal/Common/*.v`, no-Admitted and assumption scans, host, file-map, stale-status, and dot dependency gates.
