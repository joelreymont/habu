---
title: "Measure how both engines die on a foreign fault"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T13:28:15.899298+01:00"
Problem: read from the code, not run: a fault outside Habu code dumps and then dies by its own signal on x86-64 (src/habu/boot-x64.f:536-571) as on arm64 (src/habu/crash.f C-CRASH-RERAISE), so no exit status names it; EXIT-STATUS:SYSTEM is the exit only if rt_sigreturn ever returns. Acceptance: a C callback that dereferences 0 kills both engines by SIGSEGV after the dump, on Linux arm64 and x86-64, and a test holds it. Depends: none. Claim: unassigned.
