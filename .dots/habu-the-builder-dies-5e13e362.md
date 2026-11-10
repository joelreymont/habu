---
title: The builder dies of SIGSEGV when a build fails after an exit action
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T11:33:21.488651+02:00"
---

Problem: building /Users/joel/Work/reviews/habu-10x-2026-11-10/owned/heap2.f or clean1.f, whose top level calls AT-EXIT:ADD or FS:CLEANUP+ and whose build then fails, ends the builder with SIGSEGV (status 139); heap1.f, the same without the action, exits 74. Cause not determined. Found by the owned-cells hunt. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a root-cause hunt names the faulting word and why; then a failed build exits with its own status and message whatever the top level registered. Files: src/lib/at-exit.f, the build driver (tools/hb-build.f and what it loads). Verify: both cases exit as heap1.f does; a test that fails on the base. Depends: none. Ownership: the build driver's failure path. Claim: agent=bob workspace=.jj-ws.noindex/bob-exitvec

Hunt result 2026-11-10 (bob, /Users/joel/Work/reviews/habu-10x-2026-11-10/hunt-builder-segv.md): the program's copy of AT-EXIT in the build space stores its runner in the builder's process-exit vector (at-exit.f:88-90); a failed build unmaps the space (linker.f CLOSE, UNDO) and the builder's die calls the dangling runner, faulting in atomic-cas on the copy's MUTEX. The heap pointer plays no part: ADD followed by an undefined word crashes too. Fix running in bob-exitvec on master 8fc5961a: once the space has closed the vector never points into it, and every action the builder itself added still runs. A top level that ends the process while the space is open is habu-a-build-whose-3cea7b34.
