---
title: "Infer engine: sample and detokenize step"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.412930+02:00"
blocks:
  - habu-infer-engine-one-3d0453e2
---

Why this exists:
token selection and text emission must be composed without changing the canonical host sampler semantics or advancing state after output failure.

Required result:
feed the decode logit row to the host reference sampler, update deterministic RNG state transactionally, and detokenize the selected identifier.

Done when:
greedy and fixed-seed fixture sequences match canonical sampling; invalid sampling parameters and detokenizer failure do not advance engine state.

Expected touch points: new maki/infer/engine-output.f, focused test, FILEMAP.md.
Smallest check: focused sampling/detokenization transaction test.
Prerequisites: one paged decode step and landed sampling module.
Owned result: host sample and detokenize composition only.
Claim: unassigned.
