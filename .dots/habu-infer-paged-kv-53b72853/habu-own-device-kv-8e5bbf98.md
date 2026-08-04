---
title: Own device KV storage
status: active
priority: 1
issue-type: task
created-at: "2026-07-29T23:22:19.585295+02:00"
blocks:
  - habu-add-kv-layer-41961bed
  - habu-copy-within-gpu-68cd883c
---

Why: KV payload bytes are allocated in ordinary host memory while device kernels need one authenticated device region and the current caller-owned header is a public raw pointer.

Result: package KV adds public linear `KV:cache`. `OPEN ( GPU:session KV:config -- GPU:session result<KV:cache,n> )` validates the complete geometry, allocates one KV-owned host block containing the header and all metadata/table rows, allocates one `GPU:buffer` containing every layer-major physical page, and publishes only after both owners exist. `CLOSE ( GPU:session KV:cache -- GPU:session result<n,n> )` consumes the cache, attempts device release before host release, and returns the recoverable device failure if host release succeeds; a host unmap failure remains uncatchably fatal through the existing MEM ownership boundary. The hard cut deletes public `HDR-BYTES`, `INIT`, `DISPOSE`, every caller-owned cache pointer, and the host payload allocation.

All mutable operations thread the linear cache: `ALLOC-SEQ ( KV:cache n -- KV:cache result<KV:seq,n> )`, `APPEND-TOKEN ( GPU:session KV:cache KV:seq -- GPU:session KV:cache result<n,n> )`, `FORK-SEQ ( KV:cache KV:seq -- KV:cache result<KV:seq,n> )`, and `CANCEL-SEQ ( KV:cache KV:seq -- KV:cache result<n,n> )`. `APPEND-TOKEN` threads the live session because whole-page COW calls `GPU:COPY`; the cache never stores or duplicates session authority. Cache-only queries return the cache plus their current scalar; sequence queries and `PAGES-FOR` return the cache plus `result<n,n>`. Private KV code may borrow only bounded device spans while the session and cache stay live; no raw host or device pointer becomes public. `FOOTPRINT ( KV:cache -- KV:cache CAD-NUM:alloc-byte-len CAD-NUM:alloc-byte-len )` returns the stored host and device extents. All host allocation occurs inside `OPEN`; no operation after `OPEN` allocates.

Owner: KV cache lifetime, host allocator metadata, device payload, and footprint only. Production red: current descriptors cannot address K or V from a device kernel and cannot contain `GPU:buffer` without erasing its linear ownership. Acceptance: first and last layer, page, token, and head addresses match checked geometry; all spans stay inside the one device region; whole-page COW copies exact bytes; every validation, host allocation, device allocation, copy, and close failure preserves or releases each owner exactly once; `FOOTPRINT` equals the two stored extents; two caches coexist on one session; old pointer API and host payload are absent. Forbidden: trusted public pointer, unified-memory fallback, mirrored payload, per-layer allocation, registry, raw address API, version, migration path, second cache, async copy, or allocation after `OPEN`. Smallest owning check: `bin/hb --load maki/infer/kv-cache-test.f` through real `GPU:session`, `GPU:buffer`, and `GPU:COPY` on DGX Spark.

Claim: agent=codex-device-kv workspace=.jj-ws/habu-own-device-kv-8e5bbf98
