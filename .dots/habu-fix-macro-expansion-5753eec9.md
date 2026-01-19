---
title: "Fix macro expansion: macros can't access global functions"
status: open
priority: 2
issue-type: task
created-at: "2026-01-19T05:57:50.610960+02:00"
---

src/interp/repl.zig:callMacro - When macro VM copies globals, it gets 0 globals because defun doesn't populate self.vm.globals during file loading. Need to ensure globals are shared/synchronized between nested VMs and main VM. Test: tests.integration.test.stdlib compiles
