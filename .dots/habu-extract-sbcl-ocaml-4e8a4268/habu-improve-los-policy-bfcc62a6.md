---
title: Improve LOS policy and reuse
status: closed
priority: 2
issue-type: task
created-at: "\"2026-02-20T08:55:19.486227+01:00\""
closed-at: "2026-02-20T15:26:17.716215+01:00"
close-reason: All LOS child dots completed
blocks:
  - habu-improve-tenured-free-e53ce37d
---

File: src/runtime/heap.zig:1, src/runtime/gc.zig:1; cause: LOS threshold/reuse policy may inflate memory and pauses; fix: threshold tuning, segregated free lists, optional compaction strategy; why: approach OCaml/SBCL memory efficiency.
