---
title: "Fix map test failure: function parameters not resolved in calls"
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T17:53:01.413834+02:00"
---

src/tests/integration.zig:1715, src/interp/vm.zig:4447: When calling (map (lambda (x) (* x 2)) (list3 1 2 3)), the inner call (fn (car lst)) in map's body resolves fn to nil instead of the lambda parameter. Works in REPL but fails in test harness. Root cause: function parameter lookup issue when parameter is in function call position.
