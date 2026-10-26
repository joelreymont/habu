---
title: Keep the innermost ?do counters in registers
status: open
priority: 3
issue-type: task
created-at: "2026-10-26T11:28:18.271846+02:00"
blocks:
  - habu-keep-tier-0-730a5c5d
---

Experiment step 3, last (Joel 2026-10-26): 0% of zz's gap, 5% of tender's. Cache the innermost ?do index in a register; the memory frame stays home (J-I habu2.f ~2821 is 8 instructions). Then measure the real workloads and rule on the 5% bar.
