---
title: Fix CLOS failures batch1
status: open
priority: 1
issue-type: task
created-at: "2026-02-05T22:32:05.495679+01:00"
blocks:
  - habu-add-clos-repro-f4f345fc
---

Context: /Users/joel/Work/habu/src/compiler/compile.zig:1, /Users/joel/Work/habu/src/runtime/primitives/clos.zig:1, /Users/joel/Work/habu/lib/stdlib.habu:1; cause: batch1 method dispatch/combination mismatches; fix: implement <=5 mapped semantic fixes; deps: habu-add-clos-repro-f4f345fc; verification: batch1 CLOS tests pass and baseline closes those ids.
