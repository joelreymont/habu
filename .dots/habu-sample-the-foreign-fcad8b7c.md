---
title: Sample the foreign profiler check by time
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T17:54:25.108123+03:00"
---

Problem: `test/gate-debug-lib.f` GDB-PROFILER-FOREIGN runs a fixed 100000 dlsym calls and fails when the report's foreign bucket is 0. Its comment assumes some 220 ticks, but on spark (ARM64 Linux, glibc) the loop takes about 9 ms. The red land8 gate (engine `eff4ff42`, suite `native-gate-debug`) printed `samples 9 words 9 other 0 ... foreign 0 ... attributed 13`, with the rows in FFI:DLSYM-RAW, ffi-call-bounded and startup words: most of the short loop's few ticks land in the Habu FFI wrapper, so no tick inside dlsym happens by chance. The check passed 5 of 5 alone on the same engine. The profiler is not at fault; the workload is sized for a slower host.
Acceptance: the check drives the foreign loop for a sampled interval rather than a fixed call count (for example until the sample total reaches a bound, or until a clock interval passes), long enough that an empty foreign bucket is not a chance outcome on any gate host, with a bounded run time. The comment states the ticks and foreign share measured on spark. The check still fails when foreign ticks are counted as other or dropped, and when the handler dies inside foreign code.
Files: `test/gate-debug-lib.f`.
Verify: 50 consecutive runs of the check green on spark; with the handler's foreign branch sending the tick to `PROF-OTHER`, the check fails naming the foreign bucket.
Route: direct (test-only).
Ownership: krait (Intel lane).
