---
title: "Drop the storage definers' repeated word note"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:50:22.212660+01:00"
closed-at: "2026-11-16T04:07:36.245839+01:00"
---

Problem: TYPED-BUFFER (layout-buffer.f ~:421), LAYOUT-BUFFER (~:358), DEFER-LAYOUT-BUFFER (~:372), ARENA (arena.f ~:78), DYNAMIC-BUFFER (dynamic-storage.f ~:221) and HEAP (~:246) print a word note that repeats the placed token. Acceptance: no note repeats what the place shows (the SRCLOC:TOKEN$ fix). Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
