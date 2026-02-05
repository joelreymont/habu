---
title: Fix compiler/eval failures batch1
status: open
priority: 1
issue-type: task
created-at: "2026-02-05T22:32:05.512118+01:00"
blocks:
  - habu-add-compiler-eval-74528b18
---

Context: /Users/joel/Work/habu/src/compiler/compile.zig:1, /Users/joel/Work/habu/src/bytecode/emit.zig:1, /Users/joel/Work/habu/src/interp/vm.zig:1; cause: batch1 evaluator mismatches; fix: implement <=5 mapped fixes; deps: habu-add-compiler-eval-74528b18; verification: tests pass and baseline delta closes ids.
