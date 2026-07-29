---
title: Add Qwen INFER model arm
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T22:02:55.192640+02:00"
blocks:
  - habu-infer-dense-large-a2437ab1
  - habu-encode-qwen-text-cf8260df
  - habu-upload-qwen-weights-ae774802
  - habu-own-gpt-2-22b5e92b
  - habu-complete-qwen-runtime-f5fa19f2
  - habu-own-model-asset-c6f938e4
---

Why: Qwen must enter the same closed model carrier and shared engine as GPT-2. Interface: extend INFER:model with one explicit qwen2 arm carrying MDLCFG:mcfg, DEVRT:session, and QWENTOK:tokenizer. OPEN-QWEN ( ptr u8 CAD-NUM:byte-len -- INFER:model-result ) allocates one MODEL-ASSET:ws from QWENPIN capacity, opens config and tokenizer, calls BEGIN-QWEN, threads qbuild and workspace through DEVRT:LOAD-QWEN-WEIGHTS, threads that qbuild through DEVRT:BUILD-QWEN, then calls COMPLETE-QWEN on the built value. It proves tokenizer valid count 151665 fits 152064 model rows and releases the workspace before returning opened(model) or refused(model-open-error). model-open-error extends the closed GPT-2 enum with index, shard, module-build, and runtime-completion. LOAD-QWEN-WEIGHTS alone opens and closes QWENIDX and every shard source. Any load, module-build, or completion refusal calls DROP-QWEN; every refusal releases all acquired owners exactly once. Private dispatch supplies public name `qwen2.5-7b-instruct`, valid count 151665, SAMPLE:stop-set `{ 151645, 151643 }`, pad 151643, prefill, paged decode, DEVRT:QWEN-LOGITS, encode/decode, footprint, close, and batch capability one. Engine and scheduler reject batch two. Owner: maki/infer/model.f Qwen arm and dispatch only. Production red: the closed model carrier contains GPT-2 only and no product caller assembles the Qwen modules. Acceptance: the real root calls BEGIN, LOAD, BUILD, then COMPLETE exactly once and publishes with zero MODEL-ASSET, SAFET, QWENIDX, WSTORE, qbuild, qstage, policy, or host-weight owners; each module-build failure calls no later phase, publishes no model, and drops the returned qbuild; both stop identifiers finish without a device step and stored order selects the first match; two model instances coexist. Forbidden: QWENDEV weights owner, direct ADD-QWEN call, caller-visible workspace, second build owner, generation-policy owner, vtable, callback, registry, copied valid count, fallback name, host weights, second cache, session, or engine, version, or compatibility arm. Smallest owning check: bin/hb --load maki/infer/model-qwen-test.f on DGX Spark. Claim: unassigned.
