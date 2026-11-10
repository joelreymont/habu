---
title: NAMED? answers differently in a build and under --load
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T16:53:28.714017+02:00"
---

Problem: SOURCE-ROOT:NAMED? answers false for a file while tools/hb-build-core.f loads it (105, 123 load with included or required) and true for the same file under 'hb --load'. No caller reads it during a build today (feature review of habu-keep-active-src-2da0b7b5; read from the code, not run). Acceptance: NAMED? answers the same in both, or its comment and docs/forth.md say what it answers in a build. Files: src/core/include.f, tools/hb-build-core.f. Verify: one file printing NAMED? under both. Depends: none. Ownership: none. Claim: unassigned.
