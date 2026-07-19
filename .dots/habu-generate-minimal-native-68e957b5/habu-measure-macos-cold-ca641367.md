---
title: Measure macOS cold budget
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T22:50:17.954814+02:00"
---

test/run-lib.f:64-78 lowered MACOS-COLD-MS/MACOS-COLD-WALL-MS from 46000/51000 to Spark-derived 19000/21000 while explicitly saying no macOS measurement exists. Live fa17aac5 macOS runs of the four new Maki slices in parallel passed but measured core 28.44s, db 22.57s, eval 16.47s, and eval-emit 13.47s, so the 21s whole-gate wall bound is already below the suite's 28.44s long pole before other cold-gate work. Restore the last measured safe macOS pair immediately, then run repeated isolated fresh-cache full gates on macOS with normal calibration and derive a tighter elapsed/wall pair only from those measurements. Keep Spark's measured reduction target-specific. Add the budget calibration regression and exact evidence to the owning comments/docs. Acceptance: current correct cold macOS cannot time out; three or more full cold runs establish worst, headroom, calibration, load, and variance; constants are platform-specific measured bounds; full warm/cold gates and budget tests pass. Files: test/run-lib.f and focused budget tests.
