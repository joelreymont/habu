---
title: Complete queued profiler and debugger fixes
status: open
priority: 2
issue-type: task
created-at: "2026-09-10T18:03:13.402262+03:00"
---

Owner: Cedar; accepted requests relayed by Tender remain pending. Implement package-qualified profiling names, caller-chain visibility and a stop command; reject unsupported breakpoint targets before patching code. Diagnose/fix the profiling crash during XLSX/libc inflate, preserving the foreign calling-convention/register evidence rather than adding a wrapper that hides it. Use docs/debugging.md and native inspection. Coordinate test input/reproducer with Tender. Acceptance: the original crash path runs with profiling, names distinguish package words, callers and stop work, invalid breakpoint targets fail before code mutation. This is queued tooling work, separate from the minimum checked-Forth migration.
