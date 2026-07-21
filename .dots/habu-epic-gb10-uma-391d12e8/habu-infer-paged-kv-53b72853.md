---
title: "Infer: paged KV cache allocator"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-21T15:57:38.309268+02:00\""
---

The UMA-simplified heart of the engine: fixed-size KV pages over the ONE coherent pool. Page = P tokens x n_kv_heads x head_dim x 2 (K and V) x dtype bytes, P a named constant (start 16). Per-sequence block table (vector of page ids, the landed vector discipline); free list; per-page refcount for prefix sharing (fork = bump refcounts, copy-on-append only the tail page); alloc/append-token/free-sequence/fork-sequence; watermark query for the future scheduler. Storage through MEM:ALLOC-BYTES with the ownership discipline (capacity-as-ownership, consume-before-release, no leak on any failure path). Host-side data structure only in this dot - the device kernel reads pages later via the block table. Red-first: pool exhaustion rejects named; double-free structurally impossible (refcount proof); fork shares pages byte-identically then diverges only on append; append across a page boundary allocates exactly one page; watermark exact under a churn property test (random alloc/free/fork rounds, invariant: live pages == sum of table entries deduped by refcount). maki/infer/ or lib/ placement per FILEMAP conventions - decide and record.

Claim: agent=kvpage workspace=.jj-ws/fable-kvpage machine=spark (owns the paged KV cache allocator)
