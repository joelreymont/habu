---
title: Make full-gate performance verdict robust
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T16:36:50.929096+02:00"
---

Problem: on exact commit 158a3aed, all correctness phases passed but one fresh full gate reported 78224ms > 77000ms, with native engine build at 44812ms; an immediately repeated fresh isolated run passed at 69625ms with the same under-test hash and engine build at 38483ms. The current single raw wall-clock verdict cannot distinguish a marginal source regression from transient host variance, and a blind rerun can mask a real regression. Fix: implement a checked Habu performance-verdict package with periodic same-run calibration, explicit marginal/hard-fail bands, and fresh-root retry evidence for only marginal failures; decide by a documented robust rule such as median/2-of-3 while never reusing candidate or artifact caches across attempts; report correctness separately from performance and emit deterministic per-attempt phase/control/host-admission evidence. If process CPU, load, or thermal evidence needs a missing primitive, dot that typed capability instead of adding host glue. Acceptance: synthetic stable, noisy-pass, marginal-real-regression, hard-fail, calibration-drift, cache-reuse, and missing-evidence fixtures prove no single lucky retry can pass and no single noisy sample can fail; exact-tree gate reports every attempt and final rule; normal warm/cold correctness semantics remain unchanged. Files: new test/perf-verdict.f and focused test, test/run.f integration, test/gate-stats.f only for typed measurements, FILEMAP.md, docs/bootstrap.md performance contract. Verify: focused verdict fixtures, cache-root isolation assertions, host/filemap/status lints, Maki and PTX slices, repeated full native gate evidence.
