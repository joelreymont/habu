---
title: Recover framed and bulk diff toolkit
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-19T11:33:05.868466+02:00\""
---

Forensic sweep 2026-07-19: four stranded lanes held the framed/typed/bulk diff toolkit. The side-content codec files tools/diff-side-content.f, tools/diff-side-content-read.f, and their focused test subsequently landed on master in a2b35a9c; do not recover or overwrite them from the old bulk-diff lane. Remaining absent recovery scope is the framed producer/consumer integration, bulk scanner, lint/report modules, documentation, and safe publication wiring. Lanes: habu-tools-frame-diff-e98f8a6a (~25 new files: tools/diff-capture*.f, tools/lint/diff*.f, tools/diff-report*.f, docs/diff-artifact.md; bookmark recover-frame-diff, pushed to origin), habu-tools-bulk-diff-f36d0508 (bulk scanner and historical codec source; DIVERGENT change kztnqlux with complete copy 33a68ed2 at recover-bulk-diff), habu-lint-diff-recover (shared diff-parser repair; bookmark recover-lint-diff, pushed), habu-change-file-integration (alias-safe atomic replace + field schema + framed-diff ownership; bookmark recover-change-file, pushed). Re-derive remaining modules against the landed codec and current master; do not restore obsolete duplicate codec files.

