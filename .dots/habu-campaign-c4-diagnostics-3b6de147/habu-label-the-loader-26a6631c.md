---
title: Label the loader path rejection
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:25:48.615489+01:00"
closed-at: "2026-11-16T04:07:35.598915+01:00"
---

Problem: LOADER-PATH-STOP (include.f:746-772) has no label and its JSON column and span disagree; include.f:2344-2345 has an unreachable E-PATH-NUL arm, and E-PATH-RANGE has a second catcher at :225. Acceptance: the record has a label and its JSON span matches its column; the dead arm and the second catcher are gone. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
