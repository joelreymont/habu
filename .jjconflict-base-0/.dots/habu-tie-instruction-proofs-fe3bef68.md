---
title: Tie instruction proofs to production dispatch
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.631702+02:00"
---

CG-02, phase 6. formal/Common/Insn.v:96-101 omits FMOVXD/FMOVDX/FMOVDD, the other floating encoders, ORN, and >LIMM while production selects/emits them (select.f:2706-2721, emit.f:731-771,1363-1405,1693-1714); proof inventory tests count declarations inside the models, not production coverage. Fix: one exhaustive instruction definition tied structurally to production dispatch and Rocq semantics — deleting or adding an emitter form must break the composed theorem until semantics exist; generate the encoder tables from the model (or a bounded exhaustive per-operand-field sweep) so the tiling/injectivity theorems transfer to the shipped assembler. For each lowering/optimization pass: a semantic-preservation theorem OR a witness checked by a proved validator (translation validation for heuristic passes — inlining, allocation, scheduling; direct refinement for deterministic rewrites — folding, DCE, copies, peepholes). No hand-maintained coverage manifest. Compose through emitted bytes and loaded code for the supported production slice. Reconcile with habu-prove-hir-to-1be23c02, habu-synchronize-compiler-proofs-860e7476, habu-verify-emitted-arm64-efd5eb61.
