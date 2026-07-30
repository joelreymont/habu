---
title: "Infer: paged KV cache allocator"
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-21T15:57:38.309268+02:00\""
blocks:
  - habu-finalize-provisional-kv-b8b46613
  - habu-infer-kv-atomic-c402952e
---

Campaign only; do not dispatch. Commit 1835f711 landed the sole KV allocator, including declared capacity, fork, and copy-on-write. Keep that module and make its product behavior exact: remove parameter-sweep test inflation, pin HIGH-WATER, replace the circular copy-on-write check, hard-cut public sequence identity to one opaque scalar, add the layer dimension, and make multi-row append plus cancellation atomic. Package KV performs every cache mutation; INFER is its only external owner and invokes cache construction in START, admission in OPEN-SEQ, provisional execution in RUN-ROWS, aggregate reclamation in CLOSE-MANY, and teardown in STOP. SCHED, model arms, and DEVRT receive no cache carrier. RUN-ROWS retains the provisional owner while model and DEVRT see only immutable package-owned descriptor rows; it commits after successful DONE and cancels after failed DONE or QUIESCED. Cache metrics beyond the existing HIGH-WATER, page experiments, snapshot generations, leases, retryable unmap state, quantization, and a second allocator remain out of scope.

The cache is connected only through GPT2DEV/INFER transactions. No cache mutation occurs between descriptor export and device synchronization. Close when batch append/cancel failures restore committed lengths, page tables, references, reservations, free-list membership, and descriptor ownership exactly while preserving the specified monotonic HIGH-WATER, batch-of-one and mixed-row commits publish atomically, and cancellation after synchronization cannot affect a peer. Rejected metrics, snapshot, and experiment candidates remain recoverable at source commit 85a9646fd6b97e5d2cbb86d637bcf8d8ab2aece8.

Claim: unassigned.
