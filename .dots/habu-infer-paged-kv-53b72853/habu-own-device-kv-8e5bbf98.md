---
title: Own device KV storage
status: open
priority: 1
issue-type: task
created-at: "2026-07-29T23:22:19.585295+02:00"
blocks:
  - habu-add-kv-layer-41961bed
  - habu-own-persistent-inference-ecc98bdf
---

Why: KV payload bytes are allocated in ordinary host memory while device kernels need one authenticated device address. Result: hard-cut KV pool construction to take the live DEVRT session, allocate one checked session-owned device region for every layer-major page, retain allocation metadata and page tables on host, and remove the host payload duplicate. OPEN and CLOSE return the session in exact order. Private descriptors carry only the authenticated device base, extent, layer layout, and page rows needed by kernels. FOOTPRINT returns immutable host and device byte totals from stored extents. Owner: package KV device payload lifetime and footprint only. Production red: current descriptors cannot address K or V from a device kernel. Acceptance: first and last layer, page, token, and head addresses match checked geometry; all device accesses stay inside the region; partial allocation, close, and device failures release once; FOOTPRINT equals the allocation plan; two caches coexist on one session; no host payload allocation or public pointer remains. Forbidden: unified-memory fallback, mirrored payload, per-layer allocation, raw address API, version, migration path, or second cache. Smallest owning check: bin/hb --load maki/infer/kv-cache-test.f on DGX Spark.
