---
title: Improve LOS policy and reuse
status: open
priority: 2
issue-type: task
created-at: "2026-02-20T08:55:19.486227+01:00"
blocks:
  - habu-improve-tenured-free-e53ce37d
---

File: src/runtime/heap.zig:1, src/runtime/gc.zig:1; cause: LOS threshold/reuse policy may inflate memory and pauses; fix: threshold tuning, segregated free lists, optional compaction strategy; why: approach OCaml/SBCL memory efficiency.
