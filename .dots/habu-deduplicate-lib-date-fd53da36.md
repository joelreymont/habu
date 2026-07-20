---
title: Deduplicate lib/date.f and tools/date.f
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T08:59:08.777867+02:00"
---

The stdlib packaging sweep found two parallel date modules: lib/date.f and tools/date.f carry overlapping date parsing/formatting words. Consolidate into one packaged lib module (package DATE under lib/), migrate every tools/date.f caller to the qualified lib API, delete the duplicate file, and update FILEMAP.md plus any manifest rows. Keep behavior identical: run both modules' existing tests against the consolidated module before deleting anything, and carry over any words only one copy has. Found during habu-pkg-remaining-30-99dbf693; packaging of lib/date.f itself proceeds independently in that lane.
