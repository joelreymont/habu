---
title: Unify macro table by symbol identity
status: open
priority: 2
issue-type: task
created-at: "2026-01-29T10:04:59.229492+01:00"
---

Context: src/compiler/passes/p01_expand.zig:23-70, src/compiler/passes/passes.zig:66-90; cause: MacroTable uses StringHashMap + name compare; fix: use AutoHashMap(Value,Value) and compare head symbol identity; deps: none; verification: update p01_expand tests, run zig build test --filter expand
