---
title: Pass crash and profiler suites on x86-64
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.799950+03:00"
blocks:
  - habu-port-the-crash-99c87339
  - habu-port-the-profiler-97103e6e
  - habu-run-bin-hb-6378f297
---

Problem: `src/habu/prof.f:47-50` refuses to load on x86 and `src/habu/crash.f:30-36` models aarch64 frames.
Acceptance: the `native-gate-debug`, `profiler-index` and crash-format suites green on the ThinkPad; `prof.f`'s load refusal answered by the x86 frame (K10a-c).
Files: `src/habu/prof.f`, `src/habu/crash.f`, the crash-format tests.
Verify: ThinkPad: the `native-gate-debug`, `profiler-index` and crash-format suites.
Depends: habu-port-the-crash-99c87339 (K10b), habu-port-the-profiler-97103e6e (K10c), habu-run-bin-hb-6378f297 (X6).
Route: Alder (shared: src/habu/prof.f, src/habu/crash.f, the crash-format tests).
Ownership: krait (Intel lane).
Claim: unassigned.
