---
title: Implement proper error infrastructure with stack traces
status: closed
priority: 1
issue-type: feature
assignee: ""
created-at: "2025-12-08T08:12:39.864757+02:00"
closed-at: "2025-12-08T14:06:58.780068+02:00"
close-reason: ""
---

Add CL-compliant error handling that provides:
1. get-frame-pointer primitive to read x29
2. Stack walking via fp chain: [fp]=prev_fp, [fp+8]=return_addr
3. print-hex function to output addresses
4. error function that prints: message, function context, stack trace
5. Use .map file for address-to-symbol resolution

This replaces magic sys-exit codes with proper debugging output.
