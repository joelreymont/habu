---
title: Reuse source composition arenas
status: open
priority: 1
issue-type: task
created-at: "2026-07-15T23:50:06.085713+02:00"
blocks:
  - habu-add-owned-growable-a178ca95
---

Full context: SOURCE-COMPOSE allocates paths, frozen source, digests, output, map, and events on every BUILD and caps valid programs at SOURCE-ARENA-CAP. Migrate those stores to owned growable buffers and a reusable per-build arena; clear without allocating, release replaced spans, and leave only checked arithmetic or OS allocation refusal. Acceptance: valid inputs above 1 MiB compose; repeated identical BUILD has stable mapping count and byte-identical source/map/digests; failure reset preserves the next build; the capacity-negative test is retired. Files: tools/source-compose.f/test and the grow-buffer library only through its public package API.
