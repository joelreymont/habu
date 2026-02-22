---
title: Unlock generic JIT admission after post-hoist
status: open
priority: 1
issue-type: task
created-at: "2026-02-22T20:19:50.808266+01:00"
---

PLAN.md:147 + src/interp/repl.zig + bench/maxima_workload.zig. Post-hoist rebaseline shows jit_compiled=0 on Maxima. Build a generic (not workload-specific) path to raise safe JIT admission and measurable wins.
