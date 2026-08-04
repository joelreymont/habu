---
title: Own GPT-2 inference model
status: closed
priority: 2
issue-type: task
created-at: "2026-07-29T21:01:37.512850+02:00"
closed-at: "2026-08-04T15:00:48.864596+02:00"
close-reason: "Superseded by the hard-cut GPT2:model owner landed through cd95673fd996dec8e18168a44b2f17445d2d35a5: config, CUDA session, device weights, tokenizer, logits, generation, and close now have one owner. The obsolete INFER:model, MODEL-ASSET, GPT2DEV, DEVRT, and GPT2TOK composition would duplicate that lifetime and is forbidden; future engine arms consume GPT2:model directly."
blocks:
  - habu-match-gpt-2-2e478d93
  - habu-own-gpt-2-664626a8
  - habu-own-model-asset-c6f938e4
  - habu-own-sampling-value-6dc1a8cf
---

Problem: device weights, persistent session, validated config, and tokenizer need one linear carrier. Result: package INFER owns model as a closed explicit sum whose first arm carries GPT2:config, GPT2DEV:weights, DEVRT:session, and GPT2TOK:tokenizer. OPEN-GPT2 ( ptr u8 CAD-NUM:byte-len -- INFER:model-result ) allocates one MODEL-ASSET:ws from GPT2PIN capacity, threads it synchronously through config parsing, DEVRT opening, GPT2DEV:LOAD, and tokenizer opening, then releases it before returning opened(model) or refused(model-open-error). model-open-error is one closed enum: workspace, config, runtime, weights, tokenizer, source-close, or workspace-release. Every refusal releases all acquired owners exactly once. The arm's private dispatch supplies public name `gpt2`, batch capability, valid count 50257, SAMPLE:stop-set `{ 50256 }`, encode/decode, logits, footprint, and close. CLOSE releases weights, tokenizer, then session exactly once. The Qwen leaf adds one explicit arm to the same sum. Owner: maki/infer/model.f GPT-2 model lifetime and dispatch only. Production red: no value joins the persistent executor and tokenizer. Acceptance: the real root opens with zero MODEL-ASSET, SAFET, GPT2LOAD, WSTORE, or host-weight owners; overlong and unsafe roots reject before device allocation; every named failure publishes no model; two models coexist. Forbidden: caller-visible workspace, second build owner, GPT2LOAD product input, host weight retention, vtable, callback, registry, raw handle, pack, version, or compatibility surface. Smallest owning check: bin/hb --load maki/infer/model-test.f on DGX Spark. Claim: unassigned.
