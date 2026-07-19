---
title: Re-derive spark cold budgets on the healed cache
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T05:21:08.025067+02:00"
---

SPARK-COLD-MS/SPARK-COLD-WALL-MS (56000/62000, test/run-lib.f) were sized from measurements taken while the content-key cache was degraded (the 44.6s worst case; fixed by habu-compact-and-idx-37126a26, landed 5d6edc8e). Re-measure a genuinely cold gate (fresh XDG_CACHE_HOME) on the healed-cache tree several times, and set the cold pair to measured + 25% - expect ~27s cold now, so roughly 34000/38000, but derive from measurement, not this guess. macOS cold pair mirrors whatever relative headroom spark measures (still unmeasurable here - keep the assumption comment honest). One-file change + the run-budget-cal-test pins.
