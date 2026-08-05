---
title: Exit lint wrappers with a status
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T17:54:45.317617+02:00"
---

Full context: the tools/*-lint.f command-line wrappers exit via an uncaught throw when they find something. With an interactive stdin inherited, bin/hb then drops into its reader and the command APPEARS TO HANG rather than failing — observed twice this session, once costing a two-minute timeout during a bisect. With stdin redirected from /dev/null the wrapper exits with the right status. Make the wrappers exit with a status instead of an uncaught throw, so a finding never lands in the interactive reader. Acceptance: each lint wrapper run with a finding and an interactive stdin exits promptly with a nonzero status; a regression covers the interactive-stdin case.
