---
title: Add missing ARM64 dispatch IDs (27 functions)
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-14T08:03:56.668198+02:00"
closed-at: "2025-12-25 07:21:40"
close-reason: "Obsolete: Zig rewrite"
---

27 ARM64 functions in make-compiler-fenv have no dispatch handlers in init-builtin-dispatch. Add IDs for: STUR, LDUR, STRB, LDRB, MOVK, MOVN, SUBS, MUL, SDIV, NEG, AND*, ORR, EOR, LSL, LSR, ASR, MVN, CSET, BR, BLR, B.LT, B.LE, B.GT, B.GE, SVC, BRK, ENCODE
