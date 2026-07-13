---
title: "Libraries: migrate OPTION RESULT"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:16:48.654673+02:00"
blocks:
  - habu-lowering-hash-unified-586f7881
---

Own lib/adt/option.f, lib/adt/result.f, and their focused tests. Replace SUMTYPE positional payloads with full ENUM named fields while preserving OPTION:*/RESULT:* constructor package spellings, tags, generic schemas, MATCH order, and public signatures. Validate ADT and library slices.
