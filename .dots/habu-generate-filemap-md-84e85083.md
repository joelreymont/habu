---
title: Generate FILEMAP.md from file headers
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T14:05:59.635485+02:00"
---

size-review item 4 (doc removed; findings live here). Invert tools/filemap-lint.f: emit path + first header line per entry (hard one-line cap), hand-curated Agent Context section only. Kills ~1.2K lines of prose duplicating file headers + the drift class. Gate keeps it live.

CODE-REVIEW 2026-07-21 acceptance expansion: the current filemap walker does not establish complete Maki coverage, yet FILEMAP.md contains Maki assurance claims. Discover every tracked repository path in the owned source classes, including maki, nested PTX, tools, tests, bootstrap, docs, skills, and examples; require one structural owner/header classification or an explicit generated/archive rule. Generate all factual path and coverage rows from that inventory and reject hand-written entries outside the small human context section. Renames, additions, deletions, untracked generated outputs, missing headers, duplicate owners, and excluded-directory mutations must fail. The removed Zig consumer is history and must not leave generated coverage claims.
