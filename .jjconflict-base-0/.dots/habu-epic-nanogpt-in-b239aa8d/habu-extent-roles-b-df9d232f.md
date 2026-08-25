---
title: Extent roles B/T/H in SPEC surface
status: closed
priority: 2
issue-type: task
created-at: "2026-07-18T17:36:22.580117+02:00"
closed-at: "2026-07-20T13:55:24.820297+02:00"
close-reason: "Landed 2b6ad8f8: BTC-2. FREE-EXTENT: declares a free extent role; SPEC: equations carry leading batch/head indices riding every factor (S[b h i j] = Σk Q[b h i k] · K[b h j k]). Generated accessors nest batch loops around the GEMM word (two-counter discipline held); contraction over a free extent is the BTC-7 LOAD reject exit 70 via per-equation redx witnesses; dataflow exposes the replication axes; PROMOTE carries free-extent magnitudes (live TMA-box consumer is BTC-6, reported not stubbed); adjoint = batched transposed contraction via the existing derive pipeline, central-FD gradchecked with batch isolation PROVEN (perturbing batch 1 leaves batch 0's grads bit-identical). Red-first negatives: contract-over-free, mismatched free extents, batch-not-leading, free-in-elementwise (fail-closed out of scope this lane, documented). Doc section added"
---

Nominal extent roles #B #T #H as declarable integer types in the SPEC:/candidate-B surface (golden-syntax.md:56-71); GGEMM schematic grows a free non-contracted batch index; emits checked candidate-B accessor bodies, planner dataflow with the batched-contraction free extent (tma-gather.md:29-45), PROMOTE TMA-box obligations under the SMEM budget (tma-gather.md:90-92). NOTE: flat role declaration is Foundation A1 remit; typing a folded row index as a product of B and T and splitting it in the segment op needs the product/factorization capability dot. Full contract: docs/batch-sequence-design.md section 5 BTC-2.

2026-07-20 unblocked: BTC-7 product/factorization landed (b192992e, extprod/redx families + EXTPROD:/FOLD/SPLIT/JOIN + free-vs-inner contraction rule).
Claim: agent=extroles workspace=.jj-ws/fable-extroles machine=spark (owns maki/spec.f + extent surfaces + spec tests; the cross-seq checker-reject dot habu-cross-seq-contraction-34a6265f serializes BEHIND this lane - same files, and its fixture wants this surface)
