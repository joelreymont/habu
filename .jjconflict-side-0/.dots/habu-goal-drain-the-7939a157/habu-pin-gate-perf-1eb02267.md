---
title: Pin gate perf attempt to P-cores (bimodal epochs)
status: closed
priority: 1
issue-type: task
created-at: "2026-07-19T03:02:56.894631+02:00"
closed-at: "2026-07-19T04:09:44.822206+02:00"
---

The spark gate's timed perf attempt is bimodal across epochs with IDENTICAL content: same attempt sha, same commit, cool temps, clocks at cluster max, near-idle box - fast epochs 26.9-28.7s, slow epochs 42.3-49.0s (measured repeatedly 2026-07-19 01:30-03:10; commit e4b3e38e ran 26918ms at 02:05 and 49029ms at 03:05). Root-cause class: the pool (10 workers, nested 2, >10 runnable) spills the critical chain onto A725 E-cores in some epochs - the same P/E migration that caused the original calibration drift. The fresh-child cal probe (stable 87ms) is a single child and cannot see multi-child spillover, so the calibration factor never compensates. This currently makes the warm stop-line (35000) fail honest correct runs in slow epochs and forced the cold budget to carry slow-epoch headroom. Proper fix: make the timed attempt's placement deterministic - pin the perf attempt's children (or the whole gate pool) to the 10 Cortex-X925 P-cores via sched_setaffinity/taskset at spawn (identify the P-core cpu ids from cpufreq cpuinfo_max_freq 3900000; on this box at least cpu5/15/19 are P, cpu0/10 are E - enumerate properly), keeping E-cores for the OS/background. Then re-measure and TIGHTEN the warm+cold budgets back to fast-epoch reality (the cold 56000 carries slow-epoch fat that pinning should remove). The Orin (4x2, homogeneous?) and macOS profiles need their own placement story or an explicit exemption - macOS cannot sched_setaffinity; document per-profile. Until this lands, expect spurious perf hard-fails in slow epochs on spark with correctness=t.
