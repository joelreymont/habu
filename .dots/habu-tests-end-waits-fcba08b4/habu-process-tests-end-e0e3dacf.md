---
title: Process tests end their waits by a clock
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T20:03:23.033955+01:00"
closed-at: "2026-11-13T21:29:13.798440+01:00"
---

Problem: process-test.f, process-env-test.f, process-cwd-test.f and process-tree-test.f end waits by deadlines. Acceptance: each wait on another party in these files has no deadline or count; a bound its case claims is asserted (test/AGENTS.md); the files' suites pass. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-wPROC.
