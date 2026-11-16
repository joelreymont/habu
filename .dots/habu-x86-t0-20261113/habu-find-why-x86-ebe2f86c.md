---
title: Give the x86 engine breakpoints
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T11:02:43.601138+01:00"
---

Problem: the x86 kernel has no SIGTRAP breakpoint handler, so tools/hb-build-core.f:208 RUNTIME-FILES leaves debug-watch.f, stepper.f and debug.f out of x86 engines while prims.f:510 still makes and seals package DEBUG; a program's require of debug.f is rejected E-SEALED-PACKAGE (native-gate-debug 'unsupported breakpoint target'). Acceptance: x86 breakpoints behave as arm64's (int3, out-of-line persistent trampolines sized by X64DIS, rip-relative entries one-shot), every engine builds the debugger, native-gate-debug passes on both machines. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-xbp.
