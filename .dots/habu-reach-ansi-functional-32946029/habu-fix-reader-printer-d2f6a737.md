---
title: Fix reader/printer failures batch1
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-05T22:32:05.478940+01:00\\\"\""
closed-at: "2026-02-05T23:23:21.326866+01:00"
close-reason: "Fixed read/write string key semantics and #. handling paths"
blocks:
  - habu-add-reader-printer-9a30a9ff
---

Context: /Users/joel/Work/habu/src/reader/parser.zig:1, /Users/joel/Work/habu/src/interp/vm.zig:1, /Users/joel/Work/habu/src/runtime/primitives/io.zig:1; cause: batch1 reader/printer mismatches; fix: implement semantic corrections for <=5 mapped failures; deps: habu-add-reader-printer-9a30a9ff; verification: new batch1 tests pass and baseline deltas reflect closed ids.
