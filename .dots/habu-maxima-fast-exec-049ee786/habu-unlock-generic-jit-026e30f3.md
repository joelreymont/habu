---
title: Unlock generic JIT admission after post-hoist
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-22T20:19:50.808266+01:00\""
closed-at: "2026-02-24T09:43:49.900477+01:00"
close-reason: Admission unlocked; coverage now broad with 548 compiled lambdas
---

PLAN.md:147 + src/interp/repl.zig + bench/maxima_workload.zig. Post-hoist rebaseline shows jit_compiled=0 on Maxima. Build a generic (not workload-specific) path to raise safe JIT admission and measurable wins.
