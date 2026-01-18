---
title: Fix lambda compilation in macro expansion
status: open
priority: 2
issue-type: task
created-at: "2026-01-18T06:26:36.442816+02:00"
---

Files: src/compiler/compile.zig (compileLambda functions)
Root cause: lambdas inside macro bodies fail to compile during defun compilation.
Fix lambda compilation to work in nested contexts (inside mapcar, dolist, etc).
Depends: habu-find-exact-fn-33bcd6f5
Verify: REPL loads stdlib.habu without error.
Est: 45min
