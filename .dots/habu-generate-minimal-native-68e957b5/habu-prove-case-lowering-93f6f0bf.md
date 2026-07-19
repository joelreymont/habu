---
title: Prove CASE lowering parity
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T23:07:54.167250+02:00"
---

CASE slimming closed without two acceptance proofs. test/engine-suite.f:159-171 only comments the 120/172-byte targets; no executable size or disassembly assertion fails if cset/cbz returns. bootstrap/cg/forth.fs:2327-2334 still classifies patch sites only by bit 31 and J-OF at 2473-2481 still emits cset+cbz, while native habu2.f uses class-aware LPAT and cmp+b.ne. Runtime CASE tests cannot detect code-shape drift, and recovery codegen has diverged. Add exact executable one/two-arm byte and instruction fixtures for native code; mutation must fail on cset/cbz or wrong branch patching. Teach bootstrap LPAT the same typed B.cond class and mirror J-OF, then prove Gforth recovery seed, native refresh/fixpoint x2, forward-range failure, nested/default CASE, AOT/snapshot relocation, both target CODELEN attribution and full gates. Preserve bootstrap correctness and document any deliberate seed/final size difference by region rather than waiving codegen parity. Files: test/engine-suite.f, src/habu/habu2.f only if needed, bootstrap/cg/forth.fs, size attribution/gates.
