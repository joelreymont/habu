---
title: Batch/sequence tensor dimension (B,T,C) design over 2D IR
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-18T15:24:38.493706+02:00\\\"\""
closed-at: "2026-07-18T17:39:24.801259+02:00"
close-reason: "Design landed: docs/batch-sequence-design.md (Option D: 2D B*T rows B-outermost, extent roles #B/#T/#H, segment/causal attention op; interim host batch loop). Destruction-reviewed, fixes applied. 7 sub-dots minted: segment-causal-attention-5fbe00e1, extent-roles-b-df9d232f, host-batch-loop-66773b33, get-batch-loader-542f6f22, cross-seq-contraction-34a6265f, gb10-batched-attention-3055d565, extent-role-product-8e364885."
---

STRUCTURAL: the model IR + MODEL: DSL are 2D RxC only (cad.f PARSE-SHAPE name:RxC; tensor.f 2D shape). nanoGPT is (B,T,C). Folding B*T into rows breaks causal attention sequence boundaries (attention must be block-diagonal per sequence, not across the batch). Decide+implement the convention: sequence-as-rows with per-sequence attention blocks vs a real 3rd dim in tensor-value/model-ir/SPEC. Design dot; blocks composition (attention) depends on the outcome. Dep: model-ir/tensor-value/SPEC chain.

Claim: agent=btc-opus workspace=.jj-ws/habu-batch-sequence-tensor-006f25a1

Dispatch note (Mac planner, 2026-07-18): scope under this claim is the DESIGN DELIVERABLE only — the (B,T,C) convention decision with evidence (per-sequence causal-attention correctness, IR/tensor-value/SPEC: impact analysis, GB10 memory/stride layout), delivered as a docs/ design doc plus follow-up implementation sub-dots. No tensor-value/model-ir code lands under this claim: Phase-0 mandates flagship code waits for the type-family surface, and implementation follows the SPEC: chain outcome.
