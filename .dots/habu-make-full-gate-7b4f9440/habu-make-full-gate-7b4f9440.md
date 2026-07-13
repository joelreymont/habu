---
title: Make full-gate performance verdict robust
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T16:36:50.929096+02:00"
---

Problem: on exact commit 158a3aed, all correctness phases passed but one fresh full gate reported 78224ms > 77000ms, with native engine build at 44812ms; an immediately repeated fresh isolated run passed at 69625ms with the same under-test hash and engine build at 38483ms. The current single raw wall-clock verdict cannot distinguish a marginal source regression from transient host variance, and a blind rerun can mask a real regression. Fix: implement a checked Habu performance-verdict package with periodic same-run calibration, explicit marginal/hard-fail bands, and fresh-root retry evidence for only marginal failures; decide by a documented robust rule such as median/2-of-3 while never reusing candidate or artifact caches across attempts; report correctness separately from performance and emit deterministic per-attempt phase/control/host-admission evidence. If process CPU, load, or thermal evidence needs a missing primitive, dot that typed capability instead of adding host glue. Acceptance: synthetic stable, noisy-pass, marginal-real-regression, hard-fail, calibration-drift, cache-reuse, and missing-evidence fixtures prove no single lucky retry can pass and no single noisy sample can fail; exact-tree gate reports every attempt and final rule; normal warm/cold correctness semantics remain unchanged. Files: new test/perf-verdict.f and focused test, test/run.f integration, test/gate-stats.f only for typed measurements, FILEMAP.md, docs/bootstrap.md performance contract. Verify: focused verdict fixtures, cache-root isolation assertions, host/filemap/status lints, Maki and PTX slices, repeated full native gate evidence.

## Frozen verdict rule (2026-07-13)

Bracket every attempt with pre/post calibration and normalize against the
calibrated budget. More than 10% calibration drift makes the attempt invalid.
Elapsed at or below 100% is pass-band; above 100% through 110% is marginal;
above 110% is a hard fail. An initial pass needs no retry. An initial marginal
runs exactly two more attempts and passes only when at least two of three are
pass-band. Any hard fail or invalid/missing evidence fails closed.

Every admitted attempt uses distinct fresh `HB_TMP`, `XDG_CACHE_HOME`, and
`HABU_BUILD_CACHE`, proves those roots contained no artifact from a prior
attempt, and carries the same exact-tree under-test SHA. Expected cache-hit
fixtures inside one attempt remain valid and must match their committed counter
contract; they are not cross-attempt reuse. Correctness and performance are
separate verdict fields. Implement the pure policy package first, then the
runner/evidence integration; no new host primitive is currently required.
