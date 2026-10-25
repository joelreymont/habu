---
title: Write the profiler once in Habu
status: open
priority: 2
issue-type: task
created-at: "2026-10-25T22:57:08.851766+02:00"
---

Rule: docs/architecture.md:146-147 (hand-written per-platform code is only kernel primitives); the kernel list at :113-120 has no profiler. Problem: the profiler is two hand-written twins: src/habu/prof.f (1498 lines, ARM64 encoder rows, including report and JSON printing C-PROF-REP-WORDSUM ~368, EMIT-PROFREP ~621, C-PROF-JSON-ROWS ~565 and a sort C-PROF-SORT ~1068) and src/habu/prof-x64.f (1420 lines, x86 encoders: SORT, SNAP-HELPER, HANDLER, SAY,; loaded by src/habu/kernel-x64.f:38). Acceptance: index, sort, report and JSON are typed Habu written once; per platform only the SIGALRM sample handler stays as machine code; profiler output is unchanged on both targets. Coordinate with the built-in profiler dots (rg 'profiler' .dots). Files: src/habu/prof.f, src/habu/prof-x64.f, src/habu/kernel-x64.f. Verify: profiler tests on ARM64 and x86-64, test/run.f.
