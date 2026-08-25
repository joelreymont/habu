---
title: "Infer decode: vector-load paged kernel"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.368193+02:00"
blocks:
  - habu-infer-decode-paged-f188c9e8
  - habu-infer-decode-online-17d2db72
---

Why this exists:
the paged correctness baseline needs a simple transfer path before specialized TMA or asynchronous candidates.

Required result:
package DECODE-CG emits the DECODE-REF online-softmax recurrence with aligned vector global loads through the paged address calculation. Keep scalar tail loads for exact bounds and read shared physical pages through the current bounded batch descriptor. It accepts the persistent session's owned descriptors, writes caller-owned output, and allocates, uploads, compiles, and reads back nothing per call. Do not add another recurrence, contiguous comparison kernel, persisted descriptor, transfer selector, Tensor Memory Accelerator path, or asynchronous copy.

Done when:
the same logical cache in contiguous, randomly scattered, and shared-prefix pages produces equivalent outputs for short, page-edge, and full supported contexts; unaligned tail and missing-page cases reject or stay in bounds exactly.

Expected touch points: new lib/ptx/cg-decode-paged-vector.f and focused device test.
Smallest check: correctness-only GB10 parity run.
Prerequisites: paged gather iterator and host online-softmax oracle.
Owned result: vector-load paged kernel only.
Claim: unassigned.
