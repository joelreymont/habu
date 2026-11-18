---
title: "The checker arena's refused map dies bare"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T08:42:19.383367+01:00"
---

Problem: src/core/arena-pages.f:11 dies with 'checker: arena mmap failed' (EXIT-STATUS:SYSTEM) where map-anon refuses; reached by hb-build under ulimit -v 537000000 (rc 70, then 'hb-build: src/core/include.f ended the process during the build'). Acceptance: the refusal prints an E-MAP record naming the checker arena and its bytes, rc 78, as the four sites of 062f48cf do. Depends: habu-give-the-engine-062f48cf (load order). Claim: unassigned.
