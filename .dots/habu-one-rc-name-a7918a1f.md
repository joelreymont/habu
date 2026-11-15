---
title: One rc name for every boot mapping failure
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T15:05:58.413179+02:00"
---

Problem: STACK-ABI:MAP-FAIL-RC (src/habu/stack-abi.f, 78) is the stack-mapping exit, but src/habu/habu2.f:1562, :1880, :1893, :1908 write the fixed-region mapping exits as literal 78 and src/habu/boot-x64.f:39 keeps its own MAP-FAIL-RC copy: three hand-kept copies of one status. Acceptance: every boot mapping exit names one constant; rg finds no literal 78 exit. Depends: none. Claim: unassigned.
