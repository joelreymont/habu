---
title: Share the growing string-key map
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T13:16:26.499068+01:00"
---

Problem: src/lib/json-schema.f MAP-IN, MOVE-ENTRY and MAP-ROOM copy src/lib/odata/metadata.f MAP-IN, MOVE-ENTRY and MAP-GROW (a MAP in an arena, made, or doubled when one more entry would fill it past half). Acceptance: one word set makes and grows such a map and both packages call it; their tests pass. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-map.
