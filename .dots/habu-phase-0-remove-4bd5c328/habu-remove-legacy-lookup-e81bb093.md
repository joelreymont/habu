---
title: Remove legacy lookup fallback
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.049170+02:00"
---

Problem: symbol and builtin resolution still retries legacy tables, package aliases, and unqualified fallbacks. Acceptance: lookup is package-correct and identity-based only. Files: src/runtime/heap.zig:416-430,3274-3351,4684-4698; src/compiler/compile.zig:7168-7189,9436-9498,10843-10884; src/interp/repl.zig:1487-1541,1742-1767; src/interp/vm.zig:2465-2498. Verify: focused lookup tests and rg for CL-USER/COMMON-LISP retry code. Blockers: none.
