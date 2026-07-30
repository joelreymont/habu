---
title: Publish batched GPT-2 arm
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.761923+02:00"
blocks:
  - habu-infer-batch-decode-a7520e15
  - habu-batch-gpt-2-137b7fa3
  - habu-batch-gpt-2-cf63de5a
  - habu-infer-engine-sample-0f2a4ef4
---

Why: row-indexed kernels become a product batch only when the closed GPT-2 model arm composes them and advertises the tested capacity.

Result: extend only the GPT-2 model dispatch so its existing full forward composes the landed dense-row and paged-attention operations for the current RUN-ROWS descriptor, then advertise maximum batch exactly four. Stop rows are omitted before dispatch, and engine or scheduler admission rejects five or any larger requested GPT-2 batch. RUN-ROWS owns only KV, token-history, and logit commit; INFER:NEXT-MANY alone owns provisional sampling, random-state commit, detokenized-byte copy, and output publication after RUN-ROWS succeeds.

Add no per-row host loop, completion mask, second attention, cache, or descriptor, host fallback, commit, per-call allocation or compilation, benchmark framework, Qwen batching, or silent reduction. Owner: GPT-2 multi-row model dispatch and capability four only. Production red: completed row-indexed operations are not joined by the model arm. Acceptance: NEXT-MANY batches one, two, and four with mixed lengths, scattered pages, page edges, and one EOS-excluded row match independent runs; one, two, and four issue the same operation-launch count; five rejects before mutation; every injected operation failure is returned to RUN-ROWS with all committed state unchanged. Smallest owning check: bin/hb --load maki/infer/gpt2-device-batch-test.f on DGX Spark. Claim: unassigned.
