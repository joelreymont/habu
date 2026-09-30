---
title: Port the profiler reports to x86-64
status: closed
priority: 2
issue-type: task
created-at: "2026-09-30T14:48:39.739607+03:00"
closed-at: "2026-09-30T19:02:45.249198+03:00"
close-reason: "X64PROF gains the sync, the text and JSON reports, prof-row and the limit test; on the ThinkPad hb-x64-kernel-prof-report and -limit exit 0, their six captures byte-equal to the host ARM64 rows (prof-report rc 76 at ENTRY-LABEL before), 23 x86 suites pass, 138 images keep their statuses, manifest bad=0; the suite passes on spark on eff4ff42."
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

Preflight corrections (2026-09-30; override the lines above where they differ):
- Citations: `src/habu/prims.f:567,571,572`; twin `src/habu/prof.f`: printers `236-354`, reports and row `356-705`, limit test `749-753`, fold/replay/sync `972-1068`, bodies `1098-1171`.
- Reuse: `FIND,` `EDGE,` `INDEX,` (it zeroes ENT-INCL: fold first), `TIMER,` `TIMER-START,` `ARENA>RSI,`; `HELPERS,` emits the new labels. Missing: sync, printers, report walks, three bodies and rows, the limit test.
- Contracts: walks take DBASE from `PROF-DBASE` and the count from `ARN-NDICT`, never r13/r14 (WALK reuses them). Bodies keep rbx rbp r12-r15; nothing lives in rcx/r11 across `syscall`. Only bodies sync. Foreign ticks bump `PROF-TOT` and return; Habu ticks bump it, return when `PROF-LIM` is 0 or TOT < LIM signed, else dump and exit 99. `prof-row` pops n first. `X64KERNEL:REC-*` is unreachable (`kernel-x64.f:47`): name offsets 16, 24 locally.
- ARM64 bytes: at load the host engine captures its own fd 1 through a dup2'd pipe: with no arena `prof-report` `prof-json` `prof-row`; then `0 prof-on prof-off prof-reset`, state S, report, row B, JSON. Host words: global `prof-a` `prof-b`, and C, `X64K-PROF-Q:` with a name over 16 bytes; the image seeds those names, the package row and C's wid at the host's record indices (set r14), so edge order matches. S: band via `data-base`, arena via one test-local `TRUSTED:` cast; deferred B<-A x3 (caller cell: host start+4, image A's end), B<-C x2, B<-none, pc 1; inclusive-only A; other, foreign, spill, frames, dropped nonzero. `indexed` is each engine's live count: expect the host's bytes with that number replaced by the image's.
- Images (existing `-negative` stays): `hb-x64-kernel-prof-report` 0: six captures; foreign at the limit (rbp moved, `PROF-LIM` 1 stored, wait `PROF-FOREIGN`, LIM 0 before rbp returns); a report while armed re-arms 1000 us. `hb-x64-kernel-prof-limit` 0: forks like the crash suite; child: fd 1 the pipe, `3 prof-on`, spin; parent: exit 99, output starting `profiler samples 3 words `.
- Pre-change: `X64HARNESS:INIT false X64HARNESS:BOOT-OPEN, s" prof-report" X64HARNESS:CALL-ROW,` exits 76.
- Files: none new; rewrite `prof-x64.f:1-7,363`, `kernel-x64.f:3013-3018`, `docs/x86-64.md:1738-1747,1772,1788,1797` and both tables.
- Not baked. `KERNEL,` grows: rerun every `x86-64-kernel-*` suite's images.
- Boundary: `habu-stop-the-clock-83ae936a` owns RESET-BODY, RATE-BODY, TIMER,; reuse unchanged; prof-reset only after prof-off.
- Lead decisions (2026-09-30): the `indexed` field is each engine's live count, so the expected bytes are the host's own output with that one number replaced by the image's; every other byte comes from the host. The one test-local `TRUSTED:` cast to reach the arena is accepted over a new baked primitive.
