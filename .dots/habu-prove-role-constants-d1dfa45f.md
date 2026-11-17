---
title: Prove role constants valid where written, not by unreachable arms
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T10:58:12.707543+01:00"
---

Problem: src/lib/memory.f:67-81 OK-BYTE-LEN, OK-ALIGNMENT, OK-ALLOC-BYTE-LEN and src/lib/num-arithmetic.f:59-61 match a NUM:numeric-result whose arguments are compile-time constants; every arm but ok is unreachable and throws E-MEM-TOTALITY (-3202) or E-NUM-TOTALITY (-5406) at run time. AGENTS.md: no runtime checks, check statically. Acceptance: a role constant's validity is proven when the program is checked, the unreachable arms and both TOTALITY codes are deleted, and a role value that would be invalid is rejected at its source line. Depends: a design of how the checker proves a constant role value valid. Claim: unassigned.
