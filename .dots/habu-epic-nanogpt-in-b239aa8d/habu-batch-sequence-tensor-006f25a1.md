---
title: Batch/sequence tensor dimension (B,T,C) design over 2D IR
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T15:24:38.493706+02:00"
---

STRUCTURAL: the model IR + MODEL: DSL are 2D RxC only (cad.f PARSE-SHAPE name:RxC; tensor.f 2D shape). nanoGPT is (B,T,C). Folding B*T into rows breaks causal attention sequence boundaries (attention must be block-diagonal per sequence, not across the batch). Decide+implement the convention: sequence-as-rows with per-sequence attention blocks vs a real 3rd dim in tensor-value/model-ir/SPEC. Design dot; blocks composition (attention) depends on the outcome. Dep: model-ir/tensor-value/SPEC chain.
