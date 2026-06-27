---
title: "Eval matrix: live model generator + sampled pass@k"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T12:11:42.947866+02:00"
---

pass@k is over a curated 8-candidate fixture (maki/eval-fixture.f), not k stochastic samples from a generator. Drive candidate generation from a model endpoint (or a seeded sampler) and compute pass@k as P(>=1 of k samples certify+device-correct). Needs a model endpoint wired through maki/eval.f. Blocks the Habu-PTX-vs-Triton comparison's statistical validity.
