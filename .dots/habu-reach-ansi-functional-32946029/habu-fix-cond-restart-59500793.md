---
title: Fix condition/restart failures batch1
status: open
priority: 1
issue-type: task
created-at: "2026-02-05T22:32:05.487173+01:00"
blocks:
  - habu-add-cond-restart-0e4fd31b
---

Context: /Users/joel/Work/habu/src/compiler/compile.zig:1, /Users/joel/Work/habu/src/interp/vm.zig:1, /Users/joel/Work/habu/lib/stdlib.habu:1; cause: batch1 condition/restart semantic gaps; fix: implement <=5 mapped fixes; deps: habu-add-cond-restart-0e4fd31b; verification: batch1 tests pass and mapped ids close in baseline delta.
