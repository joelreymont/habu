---
title: Widen small-documents benchmark admissibility
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T17:36:37.934099+02:00"
---

Full context: test/json-read-perf-phase.f benchmark '20,000 small documents' (assert 393, budget constant in lib/json-read-perf-test.f) intermittently exceeds its calibrated budget by ~1.8 percent while the phase's calibration declares the run admissible (red on the baseline gate run at parent 960bf2d5, green on the immediately following merged-tree run). The admissibility probe (calibration workload, load average, runnable counts) did not detect the contention that inflated the median. Fix structurally, not by inflating the budget: strengthen admissibility for this benchmark (bound the accepted median by the calibration delta it already measures, or re-measure once on inadmissible variance) so a contended sample is inadmissible-and-retried rather than red. Acceptance: the benchmark still reds under its slowed-workload mutation, stays green across repeated full-suite runs, and a synthetic-load fixture grades inadmissible, per the convention in habu-add-pinned-engine-90090800.
