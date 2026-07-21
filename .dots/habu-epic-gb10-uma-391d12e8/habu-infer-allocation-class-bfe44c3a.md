---
title: "Infer: allocation-class + page-fault benchmark"
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T16:44:32.513615+02:00"
---

Plan-of-record new task (docs/inference-engine-plan.md sect 5.2): the allocation-class table made empirical. For each class (source mapping, hot immutable weights, KV pages, block-table snapshots, host control state, temp packing buffers, runtime workspaces, metrics buffers) measure the candidate backings on the GB10 (file-backed mmap prefaulted vs registered/advised system memory vs cuMemAlloc) for the class's actual access pattern, plus cold-start page-fault counts vs steady-state. Output: a committed table each class cites (owner/lifetime/alignment/access/sync/cleanup/accounting per class), consumed by the planner. Timing-lane discipline; extends the loader dot's residency experiment beyond weights.
