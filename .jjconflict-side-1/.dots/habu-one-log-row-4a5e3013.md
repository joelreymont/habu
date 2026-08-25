---
title: One log row per name, or say which answers
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T10:14:27.822529+02:00"
---

A direct NPUB:REPUBLISH caller can create duplicate log rows for one (name,wordlist) and LOG-FIND's first-match then answers the OLDER row (documented at LOG-FIND, publog lane 2026-08-10; unreachable through migration - the engine refuses a second definition of a tail). Decide: refuse the duplicate at LOG+ (structural, preferred if no caller needs re-republication) or make LOG-FIND answer the newest with a fixture pinning which. Files: src/compiler/native/publish.f. Depends: none.
