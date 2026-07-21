---
title: "Infer: continuous batching scheduler"
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T15:58:37.109397+02:00"
blocks:
  - habu-infer-paged-kv-53b72853
  - habu-infer-fused-decode-77f72ca7
  - habu-infer-fast-chunked-3d7cc5da
  - habu-infer-batched-paged-edf2d243
---

Phase 3: iteration-level scheduling - new sequences join the running batch at token granularity. Admission by KV watermark (the allocator's query) over the ONE pool - no swap tier exists on UMA, so no preemption-to-host machinery; a sequence either fits or waits. Per-step batch assembly for the decode kernel (batched single-query attention over per-sequence block tables), completion/eviction, prefix-sharing admission via the allocator's fork. Host logic with the task machinery where concurrency is real; keep host work off the decode path (shared bandwidth). Red-first: admission at exactly-fits succeeds and one-page-over waits; completion frees exactly the owned pages (refcount proof); a churn property test with random arrivals/lengths preserving watermark exactness. Also blocked on the decode kernel (paged phase B) - frontmatter.

Review incorporation 2026-07-21 (docs/inference-engine-plan.md sect 3.2 + M7) - SCOPE REWRITE: one watermark is the allocator INPUT, not the scheduling policy. The scheduler owns explicit request states (WAITING -> PREFILLING -> DECODING -> COMPLETED / CANCELLED / FAILED; WAITING -> REJECTED), token-level continuous admission, CHUNKED PREFILL (bounded chunks so long prompts never starve running decodes), per-request max-context and max-new-token limits, an explicit fairness policy (FIFO acceptable if named), backpressure, cancellation with full page cleanup, and TWO named admission profiles (reserve-declared-max strict vs incremental-with-conservative-growth) - never an implicit policy. Metrics explain why any request waits. Gates: token-boundary joins, chunked prefill bounded stalls, exactly-fit succeeds/one-page-over predictably waits or rejects, cancellation returns every page, aggregate throughput AND p95 inter-token latency both reported.
