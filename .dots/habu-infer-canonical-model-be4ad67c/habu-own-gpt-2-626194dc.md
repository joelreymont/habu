---
title: Own GPT-2 config type
status: closed
priority: 1
issue-type: task
created-at: "2026-08-03T00:51:36.164309+02:00"
closed-at: "2026-08-03T01:33:53.481943+02:00"
close-reason: "Landed 24ec7ce4: hard-cut generic MDLCFG/arch to flat GPT2:config; full Maki with 160 real tensors and native/PTX gate pass."
---

Problem: the GPT-2 configuration still uses the generic MDLCFG:mcfg name and a one-arm arch wrapper, so every caller carries a false multi-model abstraction. Result: hard-cut maki/infer/model-config.f and its test to maki/infer/gpt2-config.f and package GPT2 with type GPT2:config. Its semantic fields are datatype MAKI:datatype, nctx, nvocab, nlayer, nembd, nhead, tied, bos, eos, ln-eps, and attn-scale in that order. GPT2:BUILD accepts those fields directly, validates the existing extent/head/token/epsilon invariants, and appends the existing private cfg-proof as the single tested boundary required while public STRUCTURE necessarily emits MAKE/UNMAKE. Publish DATATYPE@, NCTX@, NVOCAB@, NLAYER@, NEMBD@, NHEAD@, TIED?, BOS@, EOS@, LN-EPS@, and ATTN-SCALE?. Delete arch, ARCH@, DTYPE@, MDLCFG, MDLCFG-ARCH, and old file names without aliases. Update GPT2TENSOR direct callers, Maki suite enrollment, and live type/GPT-2 docs atomically. Add no owner-product/checker work, model registry, second config type, compatibility, version, wrapper, default, lint, manifest, or unrelated cleanup. Owner: GPT2 config source/test plus direct GPT2TENSOR signatures/fixtures and exact live docs. Checkpoint: before edits, prove a private STRUCTURE is foreign-unnameable and publishes neither MAKE nor UNMAKE, then prove current MDLCFG/arch callers are confined to the owned files. Acceptance: old type/package/file/arm/accessor spellings are absent from live source and docs; GPT2 config builds and rejects every semantic invalid class; GPT2 tensor catalog/count/shape/slot behavior is unchanged; foreign raw proof substitution rejects; focused config/tensor, full Maki, package/typed exact-diff, and native stdlib/PTX gates pass. Smallest owning check: bin/hb --load maki/infer/gpt2-config-test.f.

Claim: agent=codex workspace=.jj-ws/habu-own-gpt-2-626194dc
