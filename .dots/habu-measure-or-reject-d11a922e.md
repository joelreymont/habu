---
title: Measure or reject every host performance budget
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:07:05.064494+02:00"
---

test/run-lib.f carries macOS budgets mirrored from another host and Jetson budgets guessed without a nonzero calibration. A performance verdict must come only from a measured profile for the exact host class, engine artifact, runner mode, and load condition. Define a versioned host-performance profile with acquisition date, sample distribution, percentile policy, calibration probe, machine identity, and validity range. Select it structurally; if no matching measured profile exists, correctness may run but performance is explicitly uncalibrated and cannot pass or fail by a guessed threshold. Re-measure the supported macOS, DGX Spark, and Jetson profiles under the canonical protocol, retaining old rows as history. Add wrong-host, stale-artifact, zero calibration, copied threshold, missing profile, load-shift, overflow, and profile-expiry tests. The existing Mac and Spark budget dots become measurement inputs, not independent constants. Files: gate performance profile/selection owners, measured rows, focused tests and generated docs. Verify calibration and performance-gate mutation suite, exact supported hosts where available, host/filemap/dot lints, and full native correctness gate.
