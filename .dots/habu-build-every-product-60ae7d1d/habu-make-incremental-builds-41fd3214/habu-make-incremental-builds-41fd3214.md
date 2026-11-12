---
title: Make incremental builds rebuild only what changed
status: open
priority: 1
issue-type: task
created-at: "2026-10-22T12:16:47.852262+03:00"
---

Problem: a one-file edit to the engine recompiles files that consumed nothing it changed; the child dots hold the defects. Acceptance: every child closed; a one-file edit to the engine recompiles that file plus only the files that consumed a changed fact from it. Claim: unassigned.
