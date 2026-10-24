---
title: Install retired records from file objects
status: open
priority: 2
issue-type: task
created-at: "2026-10-24T04:12:11.429031+03:00"
---

A retired record (undefine) keeps its code and index row, but retirement overwrites its wordlist cell with -2. Exact install needs the original wordlist recorded at retirement (FILE-RETIRE hook runs before the overwrite), install as its kind, then retire (XREF:RETIRE / GRID-RETIRE). Until then: unsupported-retired. Findings: /Users/joel/.cache/tmp/carl-file/bare-handoff.md. Workspace: none yet.
