---
title: Fix pathname/stream failures batch1
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-05T22:32:05.503900+01:00\""
closed-at: "2026-02-05T23:52:58.147336+01:00"
close-reason: Added %pathname-* primitive aliases, pathname coercion wrappers, and default pathname initialization; converted repros to conforming expectations.
blocks:
  - habu-add-pathname-stream-2228273a
---

Context: /Users/joel/Work/habu/src/runtime/primitives/io.zig:1, /Users/joel/Work/habu/lib/stdlib.habu:1, /Users/joel/Work/habu/src/runtime/objects.zig:1; cause: batch1 path/stream semantic mismatches; fix: implement <=5 mapped fixes; deps: habu-add-pathname-stream-2228273a; verification: tests pass and baseline delta closes ids.
