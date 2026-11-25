---
title: "Bring x86 code size and speed to arm64's"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T15:17:25.268893+01:00"
---

Problem: the x86-64 engine's compiled code is 3,289,334 bytes against arm64's 2,537,372 (+29.6%, tools/engine-size.f on both engines of master 94d51867), and code-bench's branch, search and fold kernels run about twice as slowly. Measured on the engine's words: 32,008 frame adjustments of zero and 21,804 that fit imm8 are imm32; 237,857 data stack accesses carry r12's SIB byte; 21,298 branches that fit rel8 are rel32; 16,073 ALU immediates that fit imm8 are imm32; the loop index lives in memory. Acceptance: x86 engine code at or below arm64's size, code-bench compared with C controls on both hosts, and every Habu application in ~/Work built, tested and run on pulsar with the same speed and size ratio as on spark. Depends: none. Claim: unassigned.
