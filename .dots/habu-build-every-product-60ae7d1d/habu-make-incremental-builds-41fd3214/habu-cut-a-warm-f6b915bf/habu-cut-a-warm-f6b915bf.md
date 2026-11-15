---
title: "Cut a warm build's cost"
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T22:36:22.404001+02:00"
---

Problem: every warm build runs the kernel build again (tools/hb-build-core.f:320 KERNEL-BUILD calls LINKER:WITH): 374 ms of a 1.95 s warm engine build that compiles nothing, measured 2026-11-13. Kernel-object reuse needs a design. The build's other costs are this dot's children.

Acceptance: a warm build whose kernel files are unchanged runs no kernel build, by that design; every child closed; a warm build equals a cold one.

Depends: none.

Claim: unassigned.
