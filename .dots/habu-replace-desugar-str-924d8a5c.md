---
title: Replace desugar string dispatch
status: open
priority: 2
issue-type: task
created-at: "2026-01-29T10:05:02.834655+01:00"
---

Context: src/compiler/passes/p02_desugar.zig:88-107; cause: std.mem.eql on symbol names; fix: add builtins.sym_let_star/sym_quasiquote and dispatch via symbol identity; deps: habu-unify-macro-table-629d5607; verification: update desugar tests, run zig build test --filter desugar
