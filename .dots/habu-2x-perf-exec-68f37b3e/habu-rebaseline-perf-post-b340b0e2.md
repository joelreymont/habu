---
title: Rebaseline perf post hoist sync
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-02-17T11:21:56.836324+01:00\\\"\""
closed-at: "2026-02-17T12:10:19.370815+01:00"
close-reason: completed
blocks:
  - habu-fix-hoist-compile-9a100641
---

Run bench-comp/bench-vm/bench after hoist API sync and record new baseline before GC optimization dot. Files: bench/comprehensive_bench.zig, bench/vm.zig, bench/results/*.json, docs/cranelift-parity.md.
