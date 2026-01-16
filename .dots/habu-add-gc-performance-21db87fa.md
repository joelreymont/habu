---
title: Add GC performance baseline
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:18:45.216011+02:00"
---

Files: tests/ or benchmark script
Measure current GC:
- Pause time (ms)
- Throughput (MB/s)
- Allocation count during GC
Record baseline before optimizations.
Dependencies: habu-add-gc-allocation-4355d636
Verification: baseline recorded in /tmp/gc-baseline.txt
