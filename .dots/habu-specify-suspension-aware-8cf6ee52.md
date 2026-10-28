---
title: Specify suspension-aware elapsed time
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T08:37:49.045936+03:00"
---

Phil Kaizen report: TIME:MONO-NS is documented only as monotonic nanoseconds; Linux x64 uses CLOCK_MONOTONIC (excludes suspended time) while macOS ARM64 reads CNTVCT_EL0, observed 15-minute host sleep expiring a model-call deadline on wake. Kaizen deadlines require real elapsed time including suspension uniformly. Establish the current supported clock contract from OS primary sources and all target implementations, then implement an explicitly documented uniform suspension-aware elapsed clock at the responsible native boundary, preserving monotonic ordering and duration arithmetic. Choose whether MONO-NS itself has that contract or an additional clearly named public word is needed based on actual existing callers; no caller workaround or wall-clock deadline substitution. Existing time/deadline E2Es first, target constants and meaningful emitted/runtime behavior on available hosts, independent review and matching native qualification for baked changes. Do not start ahead of incremental build and Otto codegen priorities.

Kernel waits measured on another clock: on linux-aarch64 mono-ns reads CNTVCT_EL0 (src/habu/habu1.f BMONONS) while TASK:STOP-FOR's sem_clockwait and TASK:SLEEP's nanosleep count CLOCK_MONOTONIC (src/lib/task.f POSIX-WAIT-NS, SLEEP-DEADLINE-AT). Across an NTP slew or a suspend the two diverge and both words return late against their mono-ns deadline, never early; x86-64 Linux is unaffected because its mono-ns is CLOCK_MONOTONIC (kernel-x64.f MONO-ROW). The chosen clock contract also names the clock these waits count on.
