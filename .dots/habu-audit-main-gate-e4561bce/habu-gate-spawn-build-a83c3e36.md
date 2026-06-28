---
title: Gate spawn build counters
status: open
priority: 2
issue-type: task
created-at: "2026-06-28T19:06:22.763280+02:00"
---

Problem: gate optimization currently relies on reading phase wall times and hunting source manually; duplicate hb launches and build artifact rebuilds are not counted by the gate. Fix: add checked instrumentation counters for top-level phase spawns, inner hb spawns, warm cache hits/misses, maker builds/cache hits, build-fixpoint candidate builds, and process-boundary-only tests. Emit a compact summary at gate end and make counters available to focused slices. Files: test/run.f, test/gate-pool.f, test/gate-common.f, test/gate-stdlib.f, tools/hb-build-lib.f, tools/warm-image-lib.f. Acceptance: full gate output reports counts without masking failures; no material timing regression; optimization dots can prove launch/build count reductions, not just wall-clock noise.
