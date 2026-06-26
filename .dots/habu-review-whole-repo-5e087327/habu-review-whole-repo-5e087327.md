---
title: Review whole repo factoring and typechecked blocks
status: open
priority: 2
issue-type: task
created-at: "2026-06-25T00:04:13.330473+02:00"
---

Whole-repo code review focused on factoring: find large/unfactored words, missing stack comments, unchecked boundaries that should be checked helpers, repeated DSL/list patterns, and files bundling unrelated concerns. Use docs/forth.md as the standard, cite file:line findings, classify correctness/perf/maintainability risk, and produce concrete refactor dots or patches. Root cause: Linux port work exposed gate/test code and emitter routines that were too large to review without manual stack juggling. Fix: audit the entire repo for small typechecked blocks and remove legacy/unfactored patterns where practical.
