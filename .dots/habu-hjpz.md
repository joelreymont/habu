---
title: Fix nil compiling to 0 instead of 6
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-06T09:01:46.053865+02:00"
closed-at: "2025-12-25 07:21:22"
close-reason: "Obsolete: Zig rewrite"
---

Root cause found: MCP compile tool was calling codegen-main incorrectly, passing fns as first arg instead of main-ir. Fixed in mcp.lisp. After MCP restart, verify:
1. (defun main () nil) compiles with C0 00 80 D2 (movz x0, #6) not 00 00 80 D2
2. *packages* is properly initialized to nil (tag 6) not fixnum 0
3. Stage 1 build works with correct nil handling
