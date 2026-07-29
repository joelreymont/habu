---
title: Compute Qwen split-half RoPE
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T22:01:45.159291+02:00"
blocks:
  - habu-infer-dense-host-4c9152ad
  - habu-open-qwen-device-0db2dea3
---

Why: Qwen rotates split halves while the landed RoPE emitter pairs adjacent lanes. Interface: package DEVRT owns ADD-QWEN-ROPE ( qbuild -- add-result ), which compiles the exact function, fills only the empty ROPE slot, and returns added(qbuild) or refused(qbuild,module-error). Its package-private builder check calls that stored slot; completed LOGITS later calls the same slot. The function transforms one query/key row using position, head size 128, and theta from MDLCFG: build inverse frequency and angle in FP32; round cosine and sine to BF16; rotate lane i with i+64 using BF16 multiplication and addition; apply one position to all 28 query heads and 4 key/value heads; leave values unchanged. Owner: sole ROPE-slot transition, exact function, and build-time owning check only. Production red: qbuild has no exact ROPE mutation and the current adjacent-pair emitter cannot match pinned BF16 rows. Acceptance: duplicate install rejects with qbuild unchanged; every pinned sparse BF16 word is bit-equal; every element of each named complete BF16 Q/K output differs by at most one adjacent BF16 representable value; NaN or Inf mismatch fails; swapped-half, FP32 cosine/sine, and delayed-rounding mutations fail; invalid position or geometry rejects before launch. Aggregate sums and BF16-to-FP32 drift cannot satisfy acceptance. Forbidden: public function handle, generic installer, adjacent-pair fallback, configurable rotary dialect, hardcoded theta, host fallback, per-call allocation, or second position authority. Smallest owning check: the focused GB10 Qwen RoPE test through DEVRT. Claim: unassigned.
