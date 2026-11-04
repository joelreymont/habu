---
title: "Keep T0's per-definition fetch buffers between definitions"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-29T04:43:33.361060+02:00"
closed-at: "2026-11-04T11:48:48.654339+02:00"
close-reason: "Census 2026-11-04, done: Landed 96342a9f \"Keep compile fact buffers between definitions\"; jerry-t0f not listed."
---

Workspace: jerry-t0f on nspsunws. A5 compile profile (zz load, 789 bodies, T0 361M over JIT): NFETCH reserves 8 dynamic buffers per definition and releases them (fetch.f 100-173, KEEP-BINDS 74M), with mmap/munmap churn (44M); together about 134M; the load bar needs about 74M less. Keep buffers across definitions and reset counts.
