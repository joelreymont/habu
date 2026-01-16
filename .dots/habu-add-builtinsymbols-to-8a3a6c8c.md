---
title: Add BuiltinSymbols to VM struct
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:21:18.356201+02:00"
---

src/interp/vm.zig: Add builtins: BuiltinSymbols field. Initialize in VM.init() via BuiltinSymbols.init(heap). <30min
