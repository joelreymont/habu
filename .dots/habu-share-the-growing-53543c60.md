---
title: Share the growing string-key map
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T13:16:26.499068+01:00"
---

Problem: src/lib/json-schema.f MAP-IN, MOVE-ENTRY and MAP-ROOM copy src/lib/odata/metadata.f MAP-IN, MOVE-ENTRY and MAP-GROW (a MAP in an arena, made or doubled before it is half full). Acceptance: one word set makes and grows such a map and both packages call it; their tests pass. Depends: habu-find-a-json-dfff2977. Claim: unassigned.
