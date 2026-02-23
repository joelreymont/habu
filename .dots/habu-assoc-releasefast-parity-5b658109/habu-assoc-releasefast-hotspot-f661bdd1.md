---
title: Assoc ReleaseFast hotspot RCA
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-02-23T08:55:25.657637+01:00\\\"\""
closed-at: "2026-02-23T09:13:26.588402+01:00"
close-reason: Profile ReleaseFast assoc and harden hot helper
---

bench/comprehensive_bench.zig assoc workload + src/jit/backend.zig assoc lowering/helpers: identify dominant ReleaseFast cost after jitAssoc/raw compare fixes, implement the next root-cause optimization, validate with ReleaseFast bench and focused tests. Depends on habu-jit-num-compare-cfdaf4dc.
