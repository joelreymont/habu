---
title: Type predicate codegen generates wrong tag comparison
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-16T13:36:43.406956+02:00"
closed-at: "2025-12-25 07:21:02"
close-reason: "Obsolete: Zig rewrite supersedes Lisp bootstrap"
---

PROCESS-PACKAGE-FORM crashes because a type predicate (likely consp) generates wrong code:
- Generated: (tag << 1 | 1) == 3, which tests tag==1
- For fixnum 0x11: low nibble=1, so 1<<1|1=3, comparison passes!
- Fixnums are incorrectly identified as passing the type check
- This causes car to be called on fixnum 8, crashing at address 0x10

Crash location: PROCESS-PACKAGE-FORM+676
x0=0x11 (fixnum 8), trying to ldr from x10=0x10

This is NOT the consp branch offset bug (already fixed). This is a different codegen bug producing wrong comparison value. Need to find which codegen path produces this pattern and fix the comparison constant.
