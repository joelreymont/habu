---
title: "Infer: fold generate.f onto sampling module"
status: closed
priority: 3
issue-type: task
created-at: "\"\\\"2026-07-21T16:44:02.479265+02:00\\\"\""
closed-at: "2026-07-21T17:44:18.989496+02:00"
close-reason: "Landed 72c60486: the nanoGPT generation example now re-exports the canonical sampling module's ops instead of carrying inline copies - 61 lines of duplicated primitive algebra deleted, four one-line delegations added, the generation-loop logic byte-for-byte unchanged. The design decision recorded: GEN-NEXT deliberately does NOT delegate to the module's dispatch word (different signature, different locked error codes, and the module's k==1 short-circuit consumes no RNG draw - delegating would silently shift the stream); only the duplicated algebra folded. Proof is the strongest kind: the example's test file is UNCHANGED and green - the committed locks pin the exact behavior. Full tests green at the merged tip"
---

Behavior-preserving follow-up from the sampling landing (b39e7dae): maki/examples/nanogpt/generate.f still carries inline GEN-ARGMAX/GEN-TEMP!/GEN-TOPK!/GEN-SAMPLE/GEN-NEXT copies of what maki/sampling.f now owns canonically. Fold the example onto the module; its committed sampling locks pin the algebra so the fold is provably behavior-preserving (locks unchanged = proof). Deliberately its own change per the no-churn corollary.

Claim: agent=genfold workspace=.jj-ws/fable-genfold machine=spark (owns folding generate.f onto maki/sampling.f, locks as proof)
