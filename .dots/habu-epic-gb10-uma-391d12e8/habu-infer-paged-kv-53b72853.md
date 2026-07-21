---
title: "Infer: paged KV cache allocator"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-21T15:57:38.309268+02:00\""
---

The UMA-simplified heart of the engine: fixed-size KV pages over the ONE coherent pool. Page = P tokens x n_kv_heads x head_dim x 2 (K and V) x dtype bytes, P a named constant (start 16). Per-sequence block table (vector of page ids, the landed vector discipline); free list; per-page refcount for prefix sharing (fork = bump refcounts, copy-on-append only the tail page); alloc/append-token/free-sequence/fork-sequence; watermark query for the future scheduler. Storage through MEM:ALLOC-BYTES with the ownership discipline (capacity-as-ownership, consume-before-release, no leak on any failure path). Host-side data structure only in this dot - the device kernel reads pages later via the block table. Red-first: pool exhaustion rejects named; double-free structurally impossible (refcount proof); fork shares pages byte-identically then diverges only on append; append across a page boundary allocates exactly one page; watermark exact under a churn property test (random alloc/free/fork rounds, invariant: live pages == sum of table entries deduped by refcount). maki/infer/ or lib/ placement per FILEMAP conventions - decide and record.

Claim: agent=kvpage workspace=.jj-ws/fable-kvpage machine=spark (owns the paged KV cache allocator)

Review incorporation 2026-07-21 (docs/inference-engine-plan.md M2): (1) SEPARATE the host ownership table from the GPU-consumed block-table snapshot and define the exact synchronization point at which the GPU may read a new snapshot - device-visible metadata must never race host mutation; (2) page size is a MEASURED lane, not frozen at 16 (named constant stays, measurement dot follows); (3) add cancellation and failed-prefill cleanup tests (no leak on any failed model step); (4) allocator metrics: total/free/shared pages, tail waste, high-water mark, per-model KV bytes-per-token; (5) test the declared maximum-context admission policy explicitly.

Base allocator LANDED 1835f711 (2026-07-21): maki/infer/kv-cache.f (package KV, new maki/infer/ home for the phase-1 modules) - geometry from an overflow-checked config record (KV-P=16 named), two owned mappings (pool + partitioned meta), capacity-as-ownership with install-under-catch and consume-before-release, DEFTYPE sequence handles, refcounted pages, COW fork proven byte-wise, boundary-append allocates exactly one page, churn property test with the recomputed-refcount invariant, negative proof (removing the refc bump reds the suite). Dot stays ACTIVE for the review-incorporation remainder above (device-visible snapshot + sync contract, cancellation/failed-prefill cleanup, page-size measurement lane, allocator metrics, max-context admission test).

Claim: agent=kvrem workspace=.jj-ws/fable-kvrem machine=spark (owns the KV review-remainder: device-visible snapshot + sync contract, cancellation cleanup, metrics, page-size measurement lane, max-context admission test)
