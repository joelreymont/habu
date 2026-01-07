---
title: Fix mutable captured variables in closures
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-09T13:46:02.608751+02:00"
closed-at: "2025-12-09T14:11:20.326343+02:00"
close-reason: ""
---

setq on captured variables doesn't mutate - closures use alist env which is immutable. Need to use mutable cells (vectors/boxes) for captured variables that are setq'd.
