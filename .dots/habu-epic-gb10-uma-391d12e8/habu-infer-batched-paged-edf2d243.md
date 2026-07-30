---
title: "Infer: batched paged decode"
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T16:44:54.214923+02:00"
blocks:
  - habu-infer-batch-decode-7df51c5c
---

Campaign only; do not dispatch. RUN-ROWS is the sole prefill/decode transaction. Separate small leaves index the existing GPT-2 dense operations and paged attention over its row descriptor; the model-arm leaf composes them and advertises the tested capacity. The shared terminal leaf drives NEXT-MANY through SCHED with real GPT-2 requests.

EOS rows are omitted on the host before launch; there is no completion mask. No per-row STEP loop, snapshot generation, prefix requirement, second descriptor/cache, Qwen batch promise, benchmark, metric, optimization, version, or compatibility API belongs here. Qwen explicitly advertises batch capacity one until a measured product requirement justifies Qwen batched kernels.
