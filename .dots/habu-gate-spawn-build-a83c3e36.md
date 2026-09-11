---
title: Gate spawn build counters
status: closed
priority: 2
issue-type: task
created-at: "2026-06-28T19:06:22.763280+02:00"
closed-at: "2026-06-28T19:41:59.060301+02:00"
close-reason: "completed: added checked gate-stats event log, focused gate-stats fixture, stdlib slice wiring, and full-gate summary; proof full native gate PASS 78900ms <= 90000ms with nonzero counts top-phase=21 boundary=93 helper-spawn=151 warm-build=2 candidate=1 maker-build=1"
---

Problem: gate optimization currently relies on reading phase wall times and hunting source manually; duplicate hb launches and build artifact rebuilds are not counted by the gate. Fix: add checked instrumentation counters for top-level phase spawns, inner hb spawns, warm cache hits/misses, maker builds/cache hits, build-fixpoint candidate builds, and process-boundary-only tests. Emit a compact summary at gate end and make counters available to focused slices. Files: test/run.f, test/gate-pool.f, test/gate-common.f, test/gate-stdlib.f, tools/hb-build-lib.f, tools/warm-image-lib.f. Acceptance: full gate output reports counts without masking failures; no material timing regression; optimization dots can prove launch/build count reductions, not just wall-clock noise.
