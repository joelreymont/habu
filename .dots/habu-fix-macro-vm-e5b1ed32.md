---
title: Fix macro VM crash with mapcar+let*
status: closed
priority: 2
issue-type: task
created-at: "\"2026-01-22T22:11:29.521633+02:00\""
---

src/interp/repl.zig:1733: macro VM crashes with TypeMismatch at ip=5 when macro body contains (let* ((vars (mapcar ...))). Error: 'macro_vm.run error: error.TypeMismatch at ip=5 / top of stack: type=.symbol'. Blocks all stdlib loading.
closed-at: "2026-01-23T03:52:08+02:00"
resolution: "Removed debug prints; macro expansion works"
