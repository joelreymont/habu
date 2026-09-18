---
title: Make core bulk byte copying efficient
status: open
priority: 2
issue-type: task
created-at: "2026-09-18T19:58:29.101942+03:00"
---

Problem: src/core/bytes.f:15-19 copies one byte per loop iteration, and tools/lint/text.f:211-212 carries a second independent byte-copy loop. Same-process warm-buffer measurements on the installed aarch64 engine, with tier-1 benchmark callers, copying a 1 MiB nonoverlapping span 64 times: BYTE-COPY took 207188/207256/206896 us; libc memcpy through the checked FUNCTION: FFI declaration took 1445/1451/1450 us. Destination bytes were compared with the source after each timed batch. This is about 143x in this microbenchmark, not an application-wide speedup claim. Acceptance: provide an efficient bulk-copy implementation at the owning core/compiler layer, keeping portable fallback and existing error/type/overlap contracts explicit; route equivalent lint copying through the shared implementation after comparing failure behavior. Benchmark small and large spans, aligned and unaligned addresses, with byte equality, canaries and meaningful boundary cases at both tiers. Avoid adding ad hoc optimized loops to consumers or changing the API merely for this benchmark. Files: src/core/bytes.f, owning backend/compiler path as the chosen design requires, tools/lint/text.f and focused copy tests. Verify: quiet-host throughput comparison, rebuild and full native suite if the baked core/compiler changes. Ownership: core byte operations. Claim: unassigned.
