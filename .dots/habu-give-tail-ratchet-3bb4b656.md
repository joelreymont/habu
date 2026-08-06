---
title: Give tail-ratchet the paired-timing cure
status: open
priority: 2
issue-type: task
created-at: "2026-08-06T08:39:01.509980+02:00"
---

test/tail-ratchet.f's 'nested child-process group time' assert (via internal-word-gate) is the third consumer of the paired/quiet-window discipline — a wall-clock assert that fired once in five full-suite runs under peer load, same class the timing lane cured for json-read-perf and the workload report (dff49640: interleaved rounds, fastest sample, measured-basis headroom, no verdicts beyond the data). Apply the same shape; no new machinery. Found 2026-08-06.
