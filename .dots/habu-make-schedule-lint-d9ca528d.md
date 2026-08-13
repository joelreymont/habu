---
title: Make schedule-lint enforce suite registration
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T20:23:22.327727+02:00"
---

Found by the locals-scope lane: schedule-lint counts scheduled files but does NOT enforce that a new test file is registered - it stays 0-finding with both of a suite's registrations removed. The passing-is-not-scheduled rule currently rests on reviewers remembering to break an assertion and watch the gate dispatcher red. Make the lint red on a test/compiler/native-*.f (or the tree's test naming convention) present on disk but absent from the fork list. Files: tools (schedule-lint owner). Depends: none.
