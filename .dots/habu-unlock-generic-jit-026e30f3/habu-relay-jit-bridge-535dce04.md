---
title: Relay JIT bridge errors without panic
status: open
priority: 1
issue-type: task
created-at: "2026-02-22T20:33:34.309009+01:00"
---

src/interp/vm.zig jitCallBridgeInvoke + JIT call path: replace panic-on-VM-error bridge with condition relay/boxed error return so safety>0 JIT admission can handle throws/conditions generically.
