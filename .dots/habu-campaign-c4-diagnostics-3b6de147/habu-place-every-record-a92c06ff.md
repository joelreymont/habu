---
title: Place every record a source holds
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:25:48.604557+01:00"
closed-at: "2026-11-16T04:07:34.985941+01:00"
---

Problem: PLACE-PROSE (rejection.f:707-709) prints '--> <input> not found' for a record with no place; a missing --load file is placed at an argv row the user never wrote (main.f:169) and adds a redundant word note. Acceptance: a record with no place prints no place line; a missing --load file's record names the file with no invented place. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
