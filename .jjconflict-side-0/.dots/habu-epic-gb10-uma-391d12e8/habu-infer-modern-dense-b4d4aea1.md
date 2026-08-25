---
title: "Infer: Qwen2.5-7B-Instruct"
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T16:44:54.193349+02:00"
blocks:
  - habu-infer-dense-full-14833530
---

Campaign only; do not dispatch. Add revision `a09a35458c702b33eeacc393d103063234e8bc28` of Qwen2.5-7B-Instruct as the second explicit arm of the already-working INFER engine. Leaves own the exact asset pin and stop/pad constants, qwen2 MDLCFG and config parser row, 339-role tensor catalog, four-shard index and bounded load, reusable byte-BPE adapter, immutable trusted reference fixture, exact eager-BF16 RMSNorm, linear, RoPE, SwiGLU, residual, and 28:4 paged-attention operations, the DEVRT arm and persistent upload, QKV, GQA, MLP/block, complete logits, model-carrier arm, and public 64-token acceptance.

The exact geometry is hidden 3584, 28 layers, 28 query heads, 4 key/value heads, head size 128, intermediate 18944, context 32768, RoPE theta 1e6, RMS epsilon 1e-6, 152064 head rows, and 151665 valid tokenizer identifiers. It uses BF16 checkpoint tensors and operation boundaries, FP32 RMS/softmax reductions, BF16 head output widened to FP32 logits, Q/K/V biases, no O/MLP bias, and untied embedding/head weights. For every BF16 kernel leaf, all stored sparse QWEN-REFERENCE probes must match exactly and each compared full-vector element may differ by at most one BF16 unit in the last place; final valid-token argmax and all 64 continuation identifiers/bytes must be exact. BF16-versus-FP32 drift is never an acceptance bound. The GPT-2 engine, KV cache, batch transaction, device session type, sampling path, and public INFER API remain the sole authorities. No pack, plugin, generic model framework, second engine/cache/BPE/reference executor, performance work, or compatibility path belongs here. Close only when the real pinned checkpoint emits the exact 64-token QWEN-REFERENCE continuation twice through public INFER and releases every owner.
