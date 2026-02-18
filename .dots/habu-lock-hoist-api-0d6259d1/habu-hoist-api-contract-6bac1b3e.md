---
title: Hoist api contract tests
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-17T22:23:16.875661+01:00\\\"\""
closed-at: "2026-02-18T23:30:47.526574+01:00"
close-reason: added hoist-enabled compile/runtime contract probe
---

src/jit/backend_api.zig + src/jit/hoist_contract.zig. Cause: upstream hoist drift can compile in stub mode but fail only when `-Duse-hoist=true`. Fix: add explicit compile/runtime hoist contract probe test and run it under hoist-enabled builds.
