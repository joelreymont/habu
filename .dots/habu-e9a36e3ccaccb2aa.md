---
title: Implement case macro
status: active
priority: 2
issue-type: task
created-at: "\"2026-01-08T07:13:52.282892+02:00\""
---

File: lib/stdlib.habu - case macro for symbol dispatch doesn't exist. Needs to expand (case x (a 1) (b 2)) into cond with eq tests. Important for clean symbol dispatch per AGENTS.md rules.
