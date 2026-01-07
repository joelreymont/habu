---
title: Implement module/package-based compilation
status: closed
priority: 1
issue-type: feature
assignee: ""
created-at: "2025-12-05T15:28:59.756377+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

Implement module system for organizing compiled code.

Features:
- defmodule/defpackage for declaring modules
- Export/import of symbols between modules
- Dependency tracking between modules
- Incremental recompilation (only changed modules)
- Module search path

This enables practical development workflow:
1. Edit one file
2. Recompile just that module
3. Reload into running image
4. Test

Without this, every change requires full recompilation.
