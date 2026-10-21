---
title: Show sampled CPU for passing suites
status: active
priority: 2
issue-type: task
created-at: "2026-10-21T04:16:09.266252+03:00"
---

The green651-row gate reports row wall time while CPU totals show3258.67s under load. Existing gate-pool CPU budget sampling already records each expensive row process-tree CPU, but discards it from green output, obscuring the responsible work. Print the existing sampled value for passing budgeted rows, clearly labeled as a last sample rather than exact final CPU. Preserve PASS line format and scheduling, add no sampling/state/framework. Run existing pool/orchestration checks plus independent review; use the next required integrated measurement to prioritize actual CPU cuts.
