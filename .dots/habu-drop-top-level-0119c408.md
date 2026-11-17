---
title: "Drop TOP-LEVEL's unread flags argument"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T07:32:13.040755+01:00"
---

Problem: src/core/top-row.f:18-25 TOP-LEVEL takes a flags argument (flg) it never reads; src/habu/interpreter.f:478 passes EVENT-FLAGS for it. Acceptance: TOP-LEVEL takes no flags argument, and EVENT-FLAGS goes too if nothing else reads it. Depends: none. Claim: unassigned.
