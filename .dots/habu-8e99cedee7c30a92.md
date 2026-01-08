---
title: Fix builtins orelse unreachable in compile.zig
status: open
priority: 2
issue-type: task
created-at: "2026-01-08T12:46:10.976711+02:00"
---

File: src/compiler/compile.zig, 10 instances at lines: 1870, 2070, 2777, 3243, 3283, 3892, 5674, 5739, 5883, 5902. Pattern: 'self.builtins orelse unreachable' assumes builtins is always set, but it can be null (see line 958: builtins = null). Fix: Change to 'self.builtins orelse return error.UninitializedBuiltins' to properly propagate error instead of crashing. Part of parent dot habu-f4845062f8c34cb9.
