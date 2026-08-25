---
title: Add Qwen model configuration
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T22:00:24.767035+02:00"
blocks:
  - habu-infer-dense-pin-36c8e45c
---

Why: the hard cut removes unused model metadata and the llama arm, but the pinned checkpoint needs exact Qwen2 semantics in the sole MDLCFG authority. Interface: package MDLCFG adds exactly one qwen2 arm, constructor, and accessors for hidden size 3584, 28 layers, 28 query heads, 4 key/value heads, head size derived exactly as 3584/28=128, intermediate size 18944, context 32768, RMS epsilon 1e-6, RoPE theta 1e6, BF16 weights, vocabulary rows 152064, config BOS 151643, and config EOS 151645. Qwen2 constructor semantics fix untied embeddings/head, Q/K/V bias present, and O/MLP bias absent; those values are not caller fields because the pinned config omits them. QWENTOK alone owns the valid identifier count; QWENPIN alone owns generation stop/pad constants. It adds no separate model-type package, valid-token copy, generic adapter table, schema version, compatibility arm, tensor names, checkpoint parser, or execution policy. Owner: maki/infer/model-config.f Qwen arm only. Production red: MDLCFG cannot represent this checkpoint after llama removal. Acceptance: the exact arm constructs and projects; every inconsistent geometry, head-row count, token bound, epsilon, theta, dtype, or tying input rejects before publication; derived head size and fixed bias semantics match the tensor census; existing GPT-2 config remains exact. Smallest owning check: bin/hb --load maki/infer/model-config-test.f. Claim: unassigned.
