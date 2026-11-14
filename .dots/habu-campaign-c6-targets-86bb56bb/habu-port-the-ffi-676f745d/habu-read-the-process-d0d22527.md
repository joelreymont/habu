---
title: Read the process exit vector in the x86-64 trampoline
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T15:05:06.365030+01:00"
blocks:
  - habu-run-exit-actions-54c95381
---

Problem: once habu-run-exit-actions-54c95381 lands, the arm64 exit vector is one per process, the main region's EXIT-HOOK-CELL; the x86-64 trampoline (src/habu/kernel-x64.f near :1904) still reads and clears the current thread's cell through DATA-REG, so on x86-64 a task's exit runs no actions. Acceptance: the exit-hook suite passes on x86-64, with its task cases. Depends: habu-run-exit-actions-54c95381. Claim: unassigned.
