---
title: Extent roles B/T/H in SPEC surface
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-18T17:36:22.580117+02:00\""
blocks:
  - habu-extent-role-product-8e364885
---

Nominal extent roles #B #T #H as declarable integer types in the SPEC:/candidate-B surface (golden-syntax.md:56-71); GGEMM schematic grows a free non-contracted batch index; emits checked candidate-B accessor bodies, planner dataflow with the batched-contraction free extent (tma-gather.md:29-45), PROMOTE TMA-box obligations under the SMEM budget (tma-gather.md:90-92). NOTE: flat role declaration is Foundation A1 remit; typing a folded row index as a product of B and T and splitting it in the segment op needs the product/factorization capability dot. Full contract: docs/batch-sequence-design.md section 5 BTC-2.

2026-07-20 unblocked: BTC-7 product/factorization landed (b192992e, extprod/redx families + EXTPROD:/FOLD/SPLIT/JOIN + free-vs-inner contraction rule).
Claim: agent=extroles workspace=.jj-ws/fable-extroles machine=spark (owns maki/spec.f + extent surfaces + spec tests; the cross-seq checker-reject dot habu-cross-seq-contraction-34a6265f serializes BEHIND this lane - same files, and its fixture wants this surface)
