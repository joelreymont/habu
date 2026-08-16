---
title: Consolidate LESSONS.md overlap
status: open
priority: 2
issue-type: task
created-at: "2026-08-16T18:58:41.830372+02:00"
---

Deferred twice, now dotted: the 2026-08-13/14 blocks overlap (probe-the-leaf x3, stale-binary x2, generated-strings x2) and 2026-08-15/16 added ~20 more blocks with recurring themes (pipeline-RC twice, load-sensitivity twice, per-record-walk blindness, mutation-earns-deletion x2). One consolidation pass: merge duplicates into single canonical entries, keep dates as a list per entry, no content loss. Run AFTER the bake-chain-15 landing (its lessons append to the same file - conflict avoidance). Text-only; gates: none beyond reading it back.
