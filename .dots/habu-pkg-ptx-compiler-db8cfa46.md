---
title: Package PTX compiler subsystems
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:18:06.025123+02:00"
---

Frozen revision f7ed6085 contains 984 production definitions outside package scope across 31 PTX files: cg-mma.f 160, opt-ir.f 98, cg.f 92, opt.f 78, ir.f 61, ad-dag.f 60, ad-dag-eval.f/ad-gen.f 45 each, cg-collective.f 45, ad.f 42, and the remaining emitter/tile/matmul/VJP files. These are active compiler dependencies of Maki and PTX tools, not a dead surface; only opt.f is explicitly dormant. Raw stems MMA-/OPTX-/EMIT-/AD-/PTXIR-/CG-/VJP- expose mutable arenas, cursors, register state, and helper words globally while persisting long pseudo-namespace names in the dictionary. This controller owns the migration map and completion census; implementation stays in small child dots by concern. End state: real packages for emitter, matmul/MMA, tile/collective, PTX IR/optimizer, VJP, and AD passes; short private tails; only documented APIs public; the existing PTX package remains the checked operation DSL rather than a catch-all. Acceptance: every legacy raw subsystem global and qualified private helper rejects, all documented qualified APIs resolve, a production census finds zero pseudo-package stems outside explicit DSL/wire boundaries, PTX text and device goldens remain byte-identical, optimizer idempotence and AD numerical checks pass, before/after dictionary-name/JIT/DATA/CODELEN measurements show the migration does not add bloat, and ptx-stdlib, Maki, fixpoint, host/package/dot, and full native gates pass. Do not dispatch until all child write sets and public API seams are explicit.
