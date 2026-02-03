---
title: Fix GC work queue
status: closed
priority: 3
issue-type: task
created-at: "\"\\\"2026-02-03T10:40:38.361136+01:00\\\"\""
closed-at: "2026-02-03T15:24:03.982940+01:00"
close-reason: Track GC work queue peak; drop unused RootSet
blocks:
  - habu-fix-compiler-env-aec7d63b
---

src/runtime/gc.zig:29-129: root_list unused; RootSet unused; maybeGrowQueues uses work_peak=self.work_list.items.len after draining => always 0, never grows. Fix: track peak len during GC; delete unused root_list/RootSet or wire them; add test that forces work_list near cap without allocating during GC. Verification: zig build test.
