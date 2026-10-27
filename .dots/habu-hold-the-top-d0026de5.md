---
title: Hold the top-level row live for word steps
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T19:23:06.912134+02:00"
---

Known limit: a top-level word step loads and saves the whole row, O(depth). No measured need now (literals are the measured case). Design if needed: Plan report 2026-10-26 (held row, step window, rerun, splice, flush at NEW/USIGS-GROW, compaction), summary in /Users/joel/.cache/tmp/carl-file/lead-todo.md.
