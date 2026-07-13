---
title: "Core records: remove checker registries"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:15:50.404414+02:00"
blocks:
  - habu-core-records-remove-31f84baf
---

Own checker registry record declarations in src/core/checker.f: symbol/effect/primitive/defer/value-record families. Replace raw structure definers with named offsets, sizes, alignments, and assertions while preserving arena, snapshot, and cache ABIs. Add focused registry growth/rollback tests.
