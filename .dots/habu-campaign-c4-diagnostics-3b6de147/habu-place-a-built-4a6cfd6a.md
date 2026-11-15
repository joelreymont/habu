---
title: Place a built token at its builder
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T15:46:32.484177+01:00"
closed-at: "2026-11-16T04:07:35.120958+01:00"
---

Problem: src/core/rejection.f PLACE-IN (:608-626) places a token of a built run inside the built text (BODY>INPUT :509-514 returns it unchanged), and MD-LOCAL-ENDED/UNBOUND records name the listing token l9 in their word note and JSON token; test/build-items.f UNREAD-CHILD and the JSON case after it fail. Acceptance: a built token is placed at its builder; a local-read record names the local by index only; build-items passes. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (integration, lands with C4).
