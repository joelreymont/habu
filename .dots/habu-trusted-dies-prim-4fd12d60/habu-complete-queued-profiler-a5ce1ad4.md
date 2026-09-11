---
title: Complete queued profiler and debugger fixes
status: open
priority: 2
issue-type: task
created-at: "2026-09-10T18:03:13.402262+03:00"
---

Owner: Cedar; accepted requests relayed by Tender remain pending. Implement package-qualified profiling names, caller-chain visibility and a stop command; reject unsupported breakpoint targets before patching code. Diagnose/fix the profiling crash during XLSX/libc inflate, preserving the foreign calling-convention/register evidence rather than adding a wrapper that hides it. Use docs/debugging.md and native inspection. Coordinate test input/reproducer with Tender. Acceptance: the original crash path runs with profiling, names distinguish package words, callers and stop work, invalid breakpoint targets fail before code mutation. This is queued tooling work, separate from the minimum checked-Forth migration.

Current AOT profiling crash (2026-09-11): Rowan independently reproduced SIGSEGV about5s after prof-on over full forced-tier1 Tender load on41df9051 in both rowan-arena and rowan-symbol lanes (x0=0xb,x1=0xe). Arena lane has a trivial-loop reducer tmp/prof-smoke.f. No repair yet. Keep this out of AOT optimization ownership: use existing samples plus focused counters/timing while a separate owner diagnoses using docs/debugging.md. Correlate with earlier requested profiler/foreign-call crash only if evidence supports a shared cause.

Reduction update from Rowan13:15UTC: profiler crash occurs with counting XT wrapper installed on NCOMP-DISPATCH:XT-CELL; without wrapper, same profiling run is clean in both tiers. SIGALRM handler, x0=14, about47s. Count/profiling runs must remain separate while diagnosing. Earlier five-second crash report is superseded by this isolated trigger.
