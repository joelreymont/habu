---
title: Pre-intern defclass keywords
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T10:24:19.095471+02:00"
---

src/compiler/compile.zig - Find Compiler.init or builtins:
1. Add BuiltinKeywords struct with kw_colon, kw_type, kw_initform fields
2. In init, intern ':' 'type' 'initform' keywords, store as Values
3. Store in compiler/builtins struct for access
Verification: Build succeeds, keywords accessible
