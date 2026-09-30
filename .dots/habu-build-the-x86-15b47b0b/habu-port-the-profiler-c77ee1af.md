---
title: Port the profiler reports to x86-64
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T14:48:39.739607+03:00"
blocks:
  - habu-port-the-profiler-97103e6e
---

Problem: `prof-report`, `prof-json` and `prof-row` (`src/habu/prims.f:556,560,561`) have no x86 body, and K10c's tick has no limit test. X5's first `ENGINE-PRIMS:COMPLETE` needs every kept row (X5's K-lane correction), and R5 needs `native-gate-debug` and `profiler-index`.
Acceptance: package `X64PROF` gains the x86 twins of `prof.f`'s report half: `LPROFSYNC` (fold, rebuild through K10c's index emitter, replay each deferred sample, its caller cell searched at cell-1); the text and JSON reports and `prof-row`, with their number, percent and name printers (`prof.f:327-796,1063-1159,1203-1262`); the tick's limit test (`prof.f:839-845`): a Habu sample at `PROF-LIM` prints the text report and exits `PROF-LIMIT-RC` 99, a foreign one returns; `PROFILER,` registers the three rows and `docs/x86-64.md` "Profiler rows" lists them.
Files: `src/habu/prof-x64.f`, `src/habu/kernel-x64.f`, `test/x86-64-kernel-prof.f`, `docs/x86-64.md`.
Verify: ThinkPad: new `x86-64-kernel-prof` images stop the clock, store known counters, and read `prof-report`, `prof-json` and `prof-row` on fd 1 byte for byte as the ARM64 engine prints that state; one more image reaches a limit and exits 99 with its report. Pre-change, `ENTRY-LABEL` dies 76 on `prof-report`. Then the lane's manifest loop.
Depends: habu-port-the-profiler-97103e6e (K10c).
Route: direct (x86-only files).
Ownership: krait (Intel lane).
Claim: unassigned.
