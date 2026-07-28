---
title: Make runtime time ratchet load-aware
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T21:54:22.035597+02:00"
---

Full context: test/gate-engine-lib.f CHECK-TIME (line 2253) fails the native engine runtime slice when elapsed exceeds BUDGET-MS. Observed red on the proofs tip at elapsed-ms=27720 vs max-ms=24480 with cal-pct=153 — the host calibration DID widen the budget by 53 percent and was not saturated (clamp is 300 percent), yet the run still overran, because the calibration spin (BUDGET-MS at line 2235) samples the host BEFORE the measured window while the rest of the parallel suite contends for the machine during it. The slice passes standalone and in less-loaded runs, so this is a contention artifact, not an engine regression. Same class as habu-widen-small-docs-60b79b43 (json-read-perf admissibility): fix structurally, not by inflating the nominal budget. Make the calibration reflect contention DURING the measured window (re-sample after, or bound the accepted elapsed by the measured in-window calibration delta), or grade a contended sample inadmissible and re-measure once. Acceptance: the ratchet still reds under a genuine slowdown mutation (its EXPECT-OVER fixtures stay meaningful), stays green across repeated full-suite runs under load, and a synthetic-load fixture grades inadmissible rather than red.
