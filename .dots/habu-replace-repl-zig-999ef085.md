---
title: Replace repl.zig symbol dispatch
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:21:18.963933+02:00"
---

src/interp/repl.zig:344,346,348,349,351,353,356: Replace std.mem.eql checks for defmacro/in-package/defpackage/eval-when with sym.eq(vm.builtins.sym_*). <20min
