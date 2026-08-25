---
title: "Infer: fused decode attention kernel"
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T15:58:14.142912+02:00"
blocks:
  - habu-infer-decode-vector-e5ac69b3
---

Campaign only; do not dispatch. Its leaves implement the one correctness-first attention path consumed by the persistent inference executor: checked GPT-2 geometry, the small DECODE-REF online-softmax oracle, address calculation over the KV batch descriptor, and one vector-load paged kernel. The later public 64-token INFER acceptance owns real-model paged proof; no duplicate direct-cache integration exists here. No full host GPT-2 forward, comparison kernel, transfer selector, Tensor Memory Accelerator path, asynchronous copy, performance table, snapshot/lease subsystem, quantization, or second attention implementation belongs here. Close when the device attention matches DECODE-REF across page boundaries.
