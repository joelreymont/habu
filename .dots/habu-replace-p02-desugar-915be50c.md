---
title: Replace p02_desugar.zig else/t dispatch
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:21:19.570345+02:00"
---

src/compiler/passes/p02_desugar.zig:66: Replace std.mem.eql checks for else/t with sym.eq(vm.builtins.sym_else) or sym.eq(vm.builtins.sym_t). <15min
