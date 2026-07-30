---
title: Execute Qwen paged GQA
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.451991+02:00"
blocks:
  - habu-exec-qwen-qkv-738d3d7b
  - habu-map-qwen-paged-426d1d76
  - habu-compute-qwen-bf16-ed55a127
---

Why: Q/K/V preparation and a synthetic 28:4 kernel do not prove the real Qwen attention projection and residual path.

Result: package DEVRT owns ADD-QWEN-ATTN ( qbuild -- add-result ), which installs the exact composition over the filled QKV, PAGED, and LINEAR slots, fills only ATTN, and returns added(qbuild) or refused(qbuild,module-error). Its package-private builder check takes qbuild, the Q row, and one immutable provisional KV descriptor; it runs the stored paged recurrence, rounds the concatenated output to BF16, resolves o_proj.weight through qbuild, calls the stored bias-free LINEAR function, and adds the incoming BF16 residual. It never mutates or retains a KV owner.

Add no public function handle, generic installer, second attention recurrence, contiguous product cache, prefix requirement, FP32 residual add, host fallback, per-head host launch loop, hidden mask, model callback, per-call allocation, or duplicated residual operator. Owner: sole ATTN-slot transition, grouped-query attention composition, and build-time owning check only. Production red: qbuild has no exact attention transition joining QKV, paged attention, and O projection. Acceptance: duplicate install rejects with qbuild unchanged; every pinned sparse BF16 word is bit-equal; every element of each named complete attention, O, and residual BF16 output differs by at most one adjacent BF16 representable value; NaN or Inf mismatch fails; first, page-edge, and full-prefix positions plus all seven query heads per KV head pass; valid-domain argmax is exact; head-map, rounding, role, and residual-width mutations fail without consuming owners. Aggregate sums and BF16-to-FP32 drift cannot satisfy acceptance. Smallest owning check: bin/hb --load maki/infer/qwen-paged-attention-test.f on DGX Spark. Claim: unassigned.
