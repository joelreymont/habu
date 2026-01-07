---
title: Store C-compatible names in debug info for lldb symbolication
status: closed
priority: 2
issue-type: task
assignee: ""
created-at: "2025-12-08T18:54:27.60542+02:00"
closed-at: "2025-12-25 07:21:22"
close-reason: "Obsolete: Zig rewrite"
---

lldb can't resolve breakpoints by symbol name because our Mach-O symbol table stores Lisp names (STRING=) but lldb expects C-mangled names (_STRING=). Either prefix symbols with underscore or add DWARF debug info with proper names.
