---
title: Report machine-stack exhaustion
status: open
priority: 2
issue-type: task
created-at: "2026-10-10T09:25:38.824514+03:00"
---

Problem: deep recursion that exhausts the machine stack kills native with SIGSEGV, rc 139, and no message. ~/.cache/tmp/carl-rsov/c2.f (`: R ( n -- n ) 1 + recurse ;  0 R`) ends rc 139 under the engine loop and the tier-1 Habu loop alike; c1.f (`TRUSTED: R ( -- ) recurse ;  R`) the same (master d5963403 build). src/habu/crash.f G-INSTALL-CRASH-X11 installs the crash handler with SA_SIGINFO only (C-SIGACTION-FRAME), with no SA_ONSTACK and no sigaltstack, so a fault on the exhausted machine stack cannot run the handler and the kernel kills the process. Every other VM stack ends `hb: stack bounds exceeded (<which>)` with ENGINE-ERROR:STACK-BOUNDS (crash.f CRS-DATA$ CRS-RET$ CRS-LOOP$; src/habu/stack-abi.f). src/habu/prof.f C-PROF-ALTSTACK already registers an alternate stack for the profiler's handler.
Acceptance: a machine-stack overflow ends `hb: stack bounds exceeded (machine)` with the ENGINE-ERROR:STACK-BOUNDS exit, no exit hook and no catch, as the other stacks do: the crash handler runs on an alternate signal stack registered before it is installed (every OS thread that runs Habu code; one registration shared with the profiler's if they can share it) and tells a machine-stack fault from the others by address. c1 and c2, at tier 0 and tier 1 and each under `catch`, join an E2E test with that output; a fault that is not a stack overflow keeps today's report. Linux arm64 and x86-64 get the same, or record which target is untested.
Files: src/habu/crash.f, src/habu/prof.f (if the alternate stack is shared), the x86-64 twin of the crash handler, src/habu/stack-abi.f (the machine stack's extent, if the handler needs it), an engine E2E test, docs/debugging.md.
Verify: native build per docs/gate.md; the new test; `bin/hb --load test/run.f`; two-generation build converges.
Depends: none. Worker: worker-max.
