---
title: Throw x86 memory faults as arm64 does
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T10:44:56.937213+01:00"
---

Problem: x86 GUARDS, (src/habu/boot-x64.f) lacks crash.f's row 2: it counts as Habu code only the kernel's text and the live region, so a linked program's caught fault dies by SIGSEGV (engine-stack-machine); it has no fetch rule (pc = fault address dumps), so GUARD-JUMP exits 102; a foreign fault in a stack guard writes E-STACK-BOUNDS where row 2 dumps and dies by the signal; and the saved-DATA-0, signal-stack and signal-handler pc cases are missing. Acceptance: engine-stack-machine and memory-fault pass on x86, but for TRAP-FAULT, which the breakpoint dot accepts. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-xr5.
