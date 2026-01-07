---
title: Add linearization pass (Tree IR → Linear IR)
status: closed
priority: 1
issue-type: feature
assignee: ""
created-at: "2025-12-06T21:44:50.278841+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

Add a nanopass that converts tree IR to linear IR (ANF/TAC style). This isolates tree recursion to one pass and makes codegen trivially iterative. Key architectural improvement for self-hosting.

Benefits:
- Codegen becomes a simple loop over linear IR
- Opens optimization opportunities (DCE, CSE, peephole)
- Each pass is simple and testable
- Industry-standard approach (LLVM, GCC, Chez Scheme)

Blocks: habu-o7fu (iterative codegen), habu-17sp (stack overflow)
