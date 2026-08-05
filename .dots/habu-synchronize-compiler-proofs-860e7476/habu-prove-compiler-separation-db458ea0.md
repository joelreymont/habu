---
title: Prove compiler separation laws
status: open
priority: 1
issue-type: task
created-at: "2026-07-27T14:03:04.446371+02:00"
blocks:
  - habu-model-compiler-heaps-dfef07ae
  - habu-define-compiler-dialect-28e79b1c
---

Scope: define formal/Common/Separation.v over the compiler heap model. Formalize disjoint heap union, read sharing, unique mutable ownership, operation footprints, alias classes, effect composition, locality, and the frame theorem. Acceptance: Rocq 9.2 proves disjoint-union laws, local load/store frame laws, non-alias preservation, and fail-closed treatment of volatile, atomic, fence, and unknown effects with no Admitted; weakening a footprint or alias premise breaks a mutation proof. Full prerequisites: habu-model-compiler-heaps-dfef07ae and habu-define-compiler-dialect-28e79b1c. Ownership: formal/Common/Separation.v and its focused proof tests only. Excludes arena lifecycle, individual optimizer-pass theorems, native/GPU lowering, and ID proofs.
