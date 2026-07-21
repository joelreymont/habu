---
title: "Infer: fold generate.f onto sampling module"
status: active
priority: 3
issue-type: task
created-at: "\"2026-07-21T16:44:02.479265+02:00\""
---

Behavior-preserving follow-up from the sampling landing (b39e7dae): maki/examples/nanogpt/generate.f still carries inline GEN-ARGMAX/GEN-TEMP!/GEN-TOPK!/GEN-SAMPLE/GEN-NEXT copies of what maki/sampling.f now owns canonically. Fold the example onto the module; its committed sampling locks pin the algebra so the fold is provably behavior-preserving (locks unchanged = proof). Deliberately its own change per the no-churn corollary.

Claim: agent=genfold workspace=.jj-ws/fable-genfold machine=spark (owns folding generate.f onto maki/sampling.f, locks as proof)
