---
title: Evict the intended FILE test artifact
status: active
priority: 2
issue-type: task
created-at: "\"2026-10-15T07:18:54.512394+03:00\""
---

Independent review of83530bcd found EVICTED-CHILD-CASE in test/native-file-replay-e2e.f builds under cache-changed-child while REMOVE-UNIT scans ROOT/cache. The artifact-found assertion fails before the intended fallback after child eviction. Pass the selected cache to the helper and keep the observable parent/child output and rebuilt-artifact assertions. This is the distinct behavior check for child hashing preserving parent source. Carl independent_file_core owns fixture repair before further production edits; matching native execution and focused independent review required.
