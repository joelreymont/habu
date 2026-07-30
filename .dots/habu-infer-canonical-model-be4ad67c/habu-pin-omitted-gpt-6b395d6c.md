---
title: Pin omitted GPT-2 semantics
status: active
priority: 2
issue-type: task
created-at: "2026-07-30T13:26:32.424609+02:00"
---

Problem: the pinned openai-community/gpt2 config omits torch_dtype, tie_word_embeddings, and scale_attn_weights, so HFCFG cannot build the complete MDLCFG value without inventing Hugging Face class-default compatibility. Result: GPT2PIN publishes DTYPE ( -- MAKI:dtype ) = MAKI-DTYPE:DF32, TIED? ( -- bool ) = true, and ATTN-SCALE? ( -- bool ) = true beside the revision artifact identity. These are resolved facts for this exact revision, not generic defaults. Owner: maki/infer/gpt2-pin.f and maki/infer/gpt2-pin-test.f only. Production red: the authenticated pinned config supplies none of these three mandatory model facts. Acceptance: the three typed APIs return the exact revision facts; the focused pin test and checker prove their result roles; HFCFG can consume them without default resolution; GPT2DEV later rejects any canonical SAFET dtype or tensor catalog that contradicts DTYPE or TIED?, while reference logits prove ATTN-SCALE?. Forbidden: config fallback/default logic, Hugging Face compatibility, schema/version field, duplicate dtype identity, untyped numeric dtype, caller override, generic model registry, lint, or unrelated pin changes. Smallest owning check: bin/hb --load maki/infer/gpt2-pin-test.f. Claim: agent=codex-gpt2-semantics workspace=.jj-ws/habu-pin-omitted-gpt-6b395d6c.
