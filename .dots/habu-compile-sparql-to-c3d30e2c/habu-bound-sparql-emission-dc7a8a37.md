---
title: Bound SPARQL emission time
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T16:11:51.610172+01:00"
---

Problem: emit.f once took superlinear time on nested subselects, long collections, object lists, paths and prefixes (25 s for 2,927 bytes); nothing shows the bound below holds on such queries. Acceptance: no query of at most 16,384 bytes compiles in over 1 s or 200 MB. Depends: none.
