---
title: Execute Qwen logits
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.457803+02:00"
blocks:
  - habu-exec-qwen-transformer-be02ad52
  - habu-infer-dense-host-4c9152ad
  - habu-compute-qwen-bf16-ed55a127
---

Why: one correct block does not prove the complete 28-layer model or its untied 152064-row head.

Result: package DEVRT owns ADD-QWEN-LOGITS ( qbuild -- add-result ), which installs the final composition over filled BLOCK, RMSNORM, and LINEAR slots, fills only LOGITS, and returns added(qbuild) or refused(qbuild,module-error). Its package-private builder check takes qbuild, one immutable provisional KV descriptor, and caller token rows; it resolves model.embed_tokens.weight through qbuild, runs exactly 28 stored BLOCK calls, applies stored final RMSNorm, and calls stored LINEAR with distinct lm_head.weight. It produces all 152064 BF16 outputs and widens them into caller-owned FP32 logit rows. After COMPLETE-QWEN, DEVRT:QWEN-LOGITS invokes this same stored LOGITS slot through the Qwen session. DEVRT validates only model vocabulary extent 152064 and never imports tokenizer validity or sampling policy; model row 152063 is valid and 152064 rejects. INFER alone scans [0,151665). Neither builder nor session call mutates or retains a KV owner.

Add no public function handle, generic installer, tied-head alias, full-model unroll, host executor, generic graph, per-layer allocation or compile, output softmax, sampling policy, vocabulary truncation, tokenizer dependency, plugin dispatch, or second logit buffer. Owner: sole LOGITS-slot transition, complete Qwen forward composition, build-time check, and final session call only. Production red: qbuild has no exact final transition executing embeddings through all 28 blocks and the untied head. Acceptance: duplicate install rejects with qbuild unchanged; every pinned sparse BF16 word is bit-equal; every element of each named complete final-norm and BF16 logit output differs by at most one adjacent BF16 representable value; NaN or Inf mismatch fails; every FP32 logit equals the widening of its accepted BF16 value; valid-domain argmax is exact; row 152063 is produced; first and last layers are load-bearing; wrong extent, generation, or launch failure preserves owners and committed KV. Aggregate sums and BF16-to-FP32 drift cannot satisfy acceptance. Smallest owning check: bin/hb --load maki/infer/qwen-device-logits-test.f on DGX Spark. Claim: unassigned.
