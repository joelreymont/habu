---
title: Own GPT-2 tensor catalog
status: closed
priority: 1
issue-type: task
created-at: "2026-07-29T23:22:19.378350+02:00"
closed-at: "2026-08-02T23:28:37.469271+02:00"
close-reason: "Landed at 3adec4ed: GPT2TENSOR owns COUNT and checked slot inverse; deleted MDLCFG census duplication. Fresh destruction review, real-checkpoint Maki, Orin, and native stdlib/PTX passed."
blocks:
  - habu-publish-owner-product-32b3f03c
---

Why: GPT-2 tensor counts and slot-to-role mapping are duplicated in MDLCFG while the direct device loader needs one authority. Result: package GPT2TENSOR remains the sole product catalog: keep COUNT, add TENSOR-ID-FOR-SLOT ( MDLCFG:mcfg CAD-NUM:index -- MDLCFG:mcfg GPT2TENSOR:tensor-id ), validate every slot against the consuming config, and delete MDLCFG V-CENSUS and its 4-plus-13-per-layer constants. The later GPT2DEV staging leaf consumes this catalog; this upstream leaf owns no device caller. The legacy GPT2LOAD host path is outside the product graph and gains no new API. Owner: maki/infer/gpt2-tensor.f and the direct MDLCFG duplicate deletion only. Production red: MDLCFG and GPT2TENSOR can disagree while their local tests pass. Acceptance: every slot zero through COUNT minus one round-trips to the exact role; one-over and wrong config reject; no product count or inverse table exists outside GPT2TENSOR; the focused GPT2TENSOR gate passes without a device loader. Forbidden: generated table, GPT2LOAD migration, host read API, version field, compatibility alias, second registry, or public raw slot. Smallest owning check: bin/hb --load maki/infer/gpt2-tensor-test.f.
Claim: agent=gpt2-catalog-cut workspace=.jj-ws/gpt2-catalog-cut
