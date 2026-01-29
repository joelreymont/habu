---
title: Implement GC finalization
status: open
priority: 2
issue-type: task
created-at: "2026-01-29T10:05:23.159075+01:00"
---

Context: src/runtime/gc.zig:132-180; cause: finalizeUnreachable disabled so streams/buffers leak; fix: implement from-space walk with boxed sizing + stream finalization; deps: none; verification: add GC finalization test, run zig build test --filter gc
