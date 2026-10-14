---
title: Reduce data-stack memory traffic
status: open
priority: 2
issue-type: task
created-at: "2026-10-14T16:55:46.580124+03:00"
---

Otto’s /tmp/otto-codegen.md and /Users/joel/.cache/tmp/dots-hbc-MZI8nh/report.md measure39,388 code bytes in x19 data-stack loads/stores and adjustments on Dots public436ff134. Investigate compiler representation and preserve stack effects, control-flow joins, calls and checked refusals while removing unnecessary traffic at the responsible lowering layer. Estimated15–20KB savings are unverified. Acceptance: same-source native program behavior and actual emitted-code/whole-image before-after evidence; focused E2E, native gate, byte convergence and Astra review. No compression or Dots edits.
