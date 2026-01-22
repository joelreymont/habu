---
title: Fix do macro expansion bug in LOOP
status: open
priority: 2
issue-type: task
created-at: "2026-01-23T00:26:39.118289+02:00"
---

src/interp/repl.zig: do macro at depth=35 receives argc=1 but requires arity=2. callMacro shows variadic arity mismatch. Occurs during stdlib load in LOOP macro expansion. Need to trace where malformed (do bindings) form without end-clause is generated. Block: stdlib load fails
