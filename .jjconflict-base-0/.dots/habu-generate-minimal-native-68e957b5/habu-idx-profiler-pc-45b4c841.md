---
title: Index profiler PC attribution
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T20:15:02.992231+02:00"
---

Frozen review f7ed6085, measured profiler overhead. The live prefix has ndict@=4295. EMIT-PROF in src/habu/prof.f:48-67 restarts at DBASE and linearly scans every 48-byte dictionary record on each 1 ms SIGALRM. A late or non-word sample executes roughly ten loop instructions per record: about 2147 records and 21000 instructions for an average hit, about 43000 for a miss, repeated 1000 times per second. The profiler therefore perturbs the code it measures and scales directly with dictionary growth to DICT-CAP 32768. Fix before arming the timer: build and validate a PC-sorted live-range index retaining the original dictionary index for counters, then use a bounded binary search in the signal handler. Define behavior for definitions or retirement after arming; never read a partially published index from the handler. Acceptance: property tests compare indexed and linear attribution for every live record, boundaries, aliases, retired rows, gaps, and post-arm dictionary mutations; handler work is logarithmic by an instruction-count or cycle benchmark; profile output is unchanged except exact accounting; both targets, signal tests, bootstrap mirror, fixpoint, and performance evidence pass.
