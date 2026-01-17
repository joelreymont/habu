---
title: Fix DeclEnv REPL crash with declare forms
status: open
priority: 2
issue-type: task
created-at: "2026-01-17T13:28:36.741781+02:00"
---

src/compiler/compile.zig:1285 - DeclEnv crashes in REPL when loading files with (declare ...) forms. Seg fault at HashMap+8 offset. Works in tests, fails in REPL. May be related to nested VM or arena allocator issue.
