---
title: Add JIT skip-reason telemetry
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-22T20:19:57.311000+01:00\""
closed-at: "2026-02-22T20:26:41.055071+01:00"
close-reason: completed
---

src/jit/candidates.zig + src/interp/vm.zig + src/interp/repl.zig + bench/maxima_workload.zig: record candidate/eligible/compiled counts and skip reasons (speed/safety/captures/args/unsupported) so jit_compiled=0 can be attributed.
