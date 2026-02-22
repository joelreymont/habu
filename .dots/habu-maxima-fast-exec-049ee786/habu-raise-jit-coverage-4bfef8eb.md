---
title: Raise JIT coverage
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-17T22:23:04.188059+01:00\""
closed-at: "2026-02-22T08:43:02.867199+01:00"
close-reason: Closed call-target and data-op JIT coverage subtasks
blocks:
  - habu-profile-maxima-hotspots-977ac23d
---

src/jit and src/interp/repl.zig. Cause: Maxima hot ops stay interpreted due unsupported IR/call patterns. Fix: add generic lowering for missing ops/function designators/lambda call targets.
