---
title: Fix FASL symbol table offset corruption
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-06T20:13:23.503211+02:00"
closed-at: "2025-12-25 07:21:12"
close-reason: "Obsolete: Zig rewrite"
---

Symbol table offsets in FASL are wrong because fnoffs is calculated using initial main-size but final bytecode uses main-code-final which may have different size when fnoffs is provided to codegen. COMPILE-EXPR-FULL offset 0x9c68 points to mid-function code (nop) instead of prologue.
