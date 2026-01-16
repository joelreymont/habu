---
title: Pass VM to Compiler
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:21:18.661630+02:00"
---

src/compiler/compile.zig: Add vm: *VM param to Compiler.init(). Update all Compiler.init() call sites (repl.zig, fasl.zig). <30min
