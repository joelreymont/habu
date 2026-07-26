---
title: Lower GPU matrix kernels
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:00:17.491444+02:00"
---

Full context: design GPU Wave D adds logical contraction, shared staging, async pipelines, warp/lane/fragment layouts, MMA tensorization, and epilogue fusion. Acceptance: fragment/order/resource validators pass and a known-good configuration meets the pinned baseline before search broadens.
