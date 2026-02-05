---
title: Fix package/readtable failures batch1
status: open
priority: 1
issue-type: task
created-at: "2026-02-05T22:32:05.520100+01:00"
blocks:
  - habu-add-pkg-readtable-a3987459
---

Context: /Users/joel/Work/habu/src/runtime/heap.zig:1, /Users/joel/Work/habu/src/reader/parser.zig:1, /Users/joel/Work/habu/lib/stdlib.habu:1; cause: batch1 package/readtable mismatches; fix: implement <=5 mapped fixes; deps: habu-add-pkg-readtable-a3987459; verification: tests pass and baseline delta closes ids.
