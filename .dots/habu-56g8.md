---
title: Convert recursive codegen to iterative for self-hosting
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-04T23:00:56.913273+02:00"
closed-at: "2025-12-06T21:26:31.593875+02:00"
close-reason: ""
---

Stage 1 self-compilation fails with stack overflow (SIGSEGV at stack limit) because codegen uses deep recursion.

The COMPILER code (not the generated code) uses deep recursion in:
- `lift-lambdas` - lambda lifting
- `codegen` - code generation 
- `flatten-code` - code flattening
- `compile-expr-full` - expression compilation

When compiling the ~300KB compiler source, recursion depth exceeds stack limit.

Solutions (pick one or combine):
1. Convert key recursive functions to iterative using explicit stack
2. Increase stack size in Mach-O wrapper (workaround)
3. Use continuation-passing style to reduce stack depth

TCO helps generated code but not the compiler itself since these aren't self-recursive tail calls.
