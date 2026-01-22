---
title: Fix do macro TypeMismatch error
status: open
priority: 2
issue-type: task
created-at: "2026-01-22T19:08:21.664632+02:00"
---

src/interp/repl.zig:44 and lib/stdlib.habu do macro. When stdlib loads, do macro expansion hits TypeMismatch at ip=11. Blocks 1 test. Pre-existing issue.
