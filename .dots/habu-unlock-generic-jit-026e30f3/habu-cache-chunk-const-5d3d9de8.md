---
title: Cache chunk const fixups per GC epoch
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-23T18:58:35.227090+01:00\\\"\""
closed-at: "2026-02-23T19:33:42.500956+01:00"
close-reason: completed in c93d2555
---

src/interp/vm.zig:11737 loadConst currently resolves forwarding on every const load. Implement a VM chunk-constant freshness cache keyed by chunk addr + gc_count, refresh all constants once per chunk per GC epoch, then fast-path raw loads. Keep generic semantics and no fallback masking. Depends on habu-specialize-stackmove-for-c4cdce70. Add focused VM regression covering forwarded const rewrite after GC and validate maxima hotspot + keyword_call perf.
