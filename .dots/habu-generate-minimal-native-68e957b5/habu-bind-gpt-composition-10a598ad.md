---
title: Bind GPT composition prerequisites
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-19T23:37:52.728072+02:00\""
---

The active habu-gpt-2-block-a9039501 claims GPT-2-small Nx full-model composition but lists only the closed toy MHA dot as a blocker. Current MHA is fixed-shape forward-only, positional embedding is B=1/pre-expanded, affine LayerNorm still has representation/device defects, batch extent products and SPEC broadcasts remain open, and habu-weight-tying-wte-ab4145da is not a dependency although GPT-2 shares wte storage with the LM head and must accumulate both gradients. Stop the active composition from presenting an untied B=1 forward toy as GPT-2. Amend its dependency/acceptance graph to require habu-complete-trainable-multi-39e26b3d, habu-complete-batched-pos-99332bf6, habu-make-affine-layernorm-ddb6d70d, habu-extent-role-product-8e364885, habu-spec-broadcast-forms-ad851424 and habu-weight-tying-wte-ab4145da, or explicitly narrow the dot/title to a non-GPT toy and create the real composition owner. Acceptance for the real owner: one shared wte/LM-head parameter, B>1/T>1, N repeated blocks, every parameter/input gradient, causal sequence isolation, host/device lowering, external deterministic golden, training loss reduction, exact dependency gate and no closed prototype used as completion proof. Files: GPT/nanoGPT dots, composition/tests and dependency lint; no feature implementation under this correction dot.

Claim: agent=gptaudit workspace=.jj-ws/fable-gptaudit machine=spark (READ-ONLY destruction audit of the closed GPT-2 composition against this dot's acceptance list; report only, orchestrator applies dot mutations)
