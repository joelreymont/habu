---
title: Fix list and list* literal-root traversal
status: closed
priority: 1
issue-type: task
created-at: "\"2026-03-08T17:08:50.316021+01:00\""
closed-at: "2026-03-08T17:41:06.572845+01:00"
close-reason: "done: added .list/.list_star traversal in src/testing/compile_chunk.zig:125-129 and src/interp/repl.zig:2857-2861; focused validation: zig build test -Dtest-filter='compileChunk JIT admits list and list* symbol literals' passed."
blocks:
  - habu-audit-literal-root-5caaebee
---

Files: src/interp/repl.zig:collectJitLiteralRoots, src/testing/compile_chunk.zig:collectJitLiteralRoots. What: add child traversal for .list and .list_star so pointer literals in list elements are registered as JIT roots. Why: backend already translates these nodes; current omission is a live stale-root/GC hazard. Verification: focused JIT compile test or direct script proving list/list* literals survive GC under compiled execution.
