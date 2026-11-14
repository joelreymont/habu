---
title: Place a rejection on a long source line
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T19:58:43.291662+01:00"
---

Problem: src/core/rejection.f PIN (:485) keeps the pinned line only when it fits PIN-CAP (4096, :463); a longer line is pinned as if no buffer held it, so its record loses the line, column and source line, a limit Habu set itself. AT likewise shows the whole line however long. Acceptance: a rejection on a line of any length is placed (file:line:column) and shows a window of the line around the token, marked where cut; no cap remains on the pinned line. Depends: none. Claim: unassigned.
