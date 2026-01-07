---
title: Fix char-at codegen (raw register number error)
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-07T08:10:48.851972+02:00"
closed-at: "2025-12-08T22:33:37.86824+02:00"
close-reason: ""
---

char-at tests fail with "Raw register numbers not allowed". Some codegen path is using numeric register IDs instead of keywords.
