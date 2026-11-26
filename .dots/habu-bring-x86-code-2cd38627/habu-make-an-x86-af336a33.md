---
title: Make an x86-64 flag in fewer instructions
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T14:23:37.003548+01:00"
---

Problem: tier 0's FLAG-END (src/habu/t0-x64.f :562) makes a flag with sete, movzx and neg where arm64 uses one csetm; ISROW, ISVAR and the other comparison words run two more instructions per call on x86-64 (up to about 3% of the CPU x86-64 spends over arm64 on loads). Acceptance: a comparison's flag costs at most two instructions after the compare, the engine fixpoint and the x86-64 suite pass. Depends: none. Claim: unassigned.
