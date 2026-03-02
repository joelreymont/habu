---
title: "Add :key to find/position/count family for Maxima paths"
status: open
priority: 3
issue-type: task
created-at: "2026-03-07T19:34:58.960548+01:00"
blocks:
  - habu-decompose-remaining-per-0c9e465d
---

lib/stdlib.habu sequence helpers. Root cause: find/find-if/position/position-if/count/count-if lack :key support required by CL and used by Maxima in several places. Fix: implement :key consistently and add regressions. Why: late-stage correctness closure under the single Maxima tree.
