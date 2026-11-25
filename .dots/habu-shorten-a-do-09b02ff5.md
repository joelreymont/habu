---
title: "Shorten a DO/LOOP frame's push and pop"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T18:28:51.596706+01:00"
---

Problem: x86 LOOP-POP (src/habu/t0-x64.f) is 12 bytes where 'add qword [rbx+LOOPSP-CELL], -16' is 5 once the assembler encodes an add of an imm8 to memory, and arm64 LOOP-PUSH (src/habu/t0-arm64.f) stores index and limit apart where one STP does. Acceptance: both emit the short form, programs are smaller, and a nested loop is no slower on pulsar or spark. Depends: none. Claim: unassigned.
