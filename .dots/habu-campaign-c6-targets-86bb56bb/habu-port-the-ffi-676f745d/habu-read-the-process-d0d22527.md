---
title: Read the process exit vector in the x86-64 trampoline
status: closed
priority: 2
issue-type: task
created-at: "2026-11-14T15:05:06.365030+01:00"
closed-at: "2026-11-27T14:48:18.497582+01:00"
---

Problem: the arm64 exit vector is one per process, the main region's EXIT-HOOK-CELL, and the thread that claims EXIT-OWNER-CELL runs it while any other exiting thread blocks (src/habu/habu2.f EMIT-EXITHOOK). The x86-64 trampoline (src/habu/kernel-x64.f near :1904) still reads and clears the current thread's cell through DATA-REG and claims nothing, so on x86-64 a task's exit runs no actions, and a second thread's exit ends the process while the first runs them. Acceptance: the exit-hook suite passes on x86-64, with its task and owner cases. Depends: none. Claim: unassigned.
