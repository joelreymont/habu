---
title: Find why the x86-64 assembler costs 7x the arm64 one
status: closed
priority: 2
issue-type: task
created-at: "2026-11-26T14:23:36.990211+01:00"
closed-at: "2026-11-26T19:11:02.891994+01:00"
---

Problem: in a load of all src/lib, X64ASM takes 21 ms against A64ASM's 2.8 ms (profiles /tmp/jerry-p1/x86cpu/{arm,x86}/p-N.txt). Acceptance: the cause is named with path:line and its fix dotted. Depends: none. Claim: unassigned.
