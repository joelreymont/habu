---
title: Move wide cells in pairs on x86
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T17:47:58.501796+01:00"
---

Problem: T0 moves a wide value two cells at a time on ARM (ldp, stp) and one cell at a time on x86 (src/habu/t0-x64.f), so wide moves cost x86 about twice the instructions. Acceptance: code-bench's wide cases are no slower on x86 relative to ARM than its other cases, and tenderd's x86 code does not grow. Depends: none. Claim: unassigned.
