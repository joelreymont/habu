---
title: Audit substrate proofs against the worth test
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T19:55:03.542569+02:00"
---

Full context: AGENTS.md now carries a Proof Integrity (BLOCKING) section. Apply it retroactively to formal/Common/{Interning,Structure,Storage}.v — 176 published results between them. For each: could a plausible change to src/compiler/ir/*.f falsify it? Mutate the CODE, not the model. Classify KEEP (name the falsifying code change), DEMOTE to Lemma and drop from the gate manifest, DELETE, or STRENGTHEN. Known failing example to start from: Storage.v arena_push_appends proves apush appends when apush is DEFINED as appending — it unfolds to a tautology about the model and says nothing about IR-ARENA:PUSH. Counterexamples and negative results are KEEP by construction. Measured baseline: Interning 62 results with 9 counterexamples, Structure 64 with 27, Storage 50 with 7 (named as Examples rather than by the counterexample convention). Then update test/compiler/ir-{intern,structure,storage}-proof.f and their manifests so the published set matches exactly what survived — and verify a gate still naming a removed result FAILS before fixing it, so the binding is proven real. Do not cut a result because it reads simply: well-foundedness, partition-from-contiguity, ceiling atomicity and the ABA/aliasing results are load-bearing. Acceptance: every surviving published result has a recorded falsifying code mutation; all five parity gates green.
