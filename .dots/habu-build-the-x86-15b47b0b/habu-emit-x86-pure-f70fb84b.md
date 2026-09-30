---
title: Emit x86 pure-op bodies from HIR
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.551789+03:00"
blocks:
  - habu-emit-x86-frame-d8d25223
  - habu-render-x86-neg-43e4e8f8
  - habu-render-x86-idiv-9c6d9516
  - habu-render-x86-trap-33eba82b
---

Problem: the kernel has no bodies for the pure-op rows of `src/habu/prims.f`.
Acceptance: the arithmetic/compare/shuffle rows and the memory rows (`@ ! +! c@ c! ptr-field byte-view cell-view count cells cell+ chars char+`) get x86 bodies by building a one-op HIR module per row and running the x86 rows (as `test/compiler/x64-chain.f` builds modules), so callable and inline semantics share one lowering; `test/prim-parity.f`'s case sets run natively through C8-style images (the parity file itself needs an engine; until X5 the cases are data for routine images); the rows join the `docs/x86-64.md` kernel inventory. Alternative: hand-written bodies (~150 lines); the HIR route is recommended because the parity gate then proves the compiler's own lowering of every op.
Files: `src/habu/kernel-x64.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md` (kernel inventory).
Verify: ThinkPad: routine images carrying the parity case sets.
Depends: habu-emit-x86-frame-d8d25223 (C1), habu-render-x86-neg-43e4e8f8 (C2), habu-render-x86-idiv-9c6d9516 (C3), habu-render-x86-trap-33eba82b (C5), habu-boot-and-exit-367c46f5 (K3), habu-share-the-primitive-58c235e5 (K4).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
