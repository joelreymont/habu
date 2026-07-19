---
title: Recover framed and bulk diff toolkit
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-19T11:33:05.868466+02:00\""
---

Forensic sweep 2026-07-19: four stranded lanes hold the framed/typed/bulk diff toolkit; both governing dots are OPEN on master and every distinctive file is absent from master. Lanes: habu-tools-frame-diff-e98f8a6a (~25 new files: tools/diff-capture*.f, tools/lint/diff*.f, tools/diff-report*.f, docs/diff-artifact.md; bookmark recover-frame-diff, pushed to origin), habu-tools-bulk-diff-f36d0508 (tools/bulk-diff-scan*.f, tools/diff-side-content*.f, docs/diff-side-content.md; DIVERGENT change kztnqlux with two commit copies - keep the complete copy 33a68ed2 = bookmark recover-bulk-diff (pushed), abandon copy 18e78789 during cleanup), habu-lint-diff-recover (shared diff-parser repair; bookmark recover-lint-diff, pushed), habu-change-file-integration (alias-safe atomic replace + field schema + framed-diff ownership; bookmark recover-change-file, pushed).

Claim: agent=diff-recover workspace=.jj-ws/habu-recover-framed-and-32309120
