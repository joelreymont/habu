---
title: Generate FILEMAP.md from file headers
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T14:05:59.635485+02:00"
---

size-review item 4 (doc removed; findings live here). Invert tools/filemap-lint.f: emit path + first header line per entry (hard one-line cap), hand-curated Agent Context section only. Kills ~1.2K lines of prose duplicating file headers + the drift class. Gate keeps it live.
