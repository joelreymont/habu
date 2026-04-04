---
title: Finish structure defaults and accessors
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.158300+02:00\""
closed-at: "2026-04-04T17:09:45.098380+02:00"
close-reason: "done earlier: default initforms, copy-structure, BOA/multiple constructors, accessors/setf, and print-function coverage are in tree; see src/tests/integration.zig defstruct regressions and lib/stdlib.habu/src/compiler/compile.zig cutover"
blocks:
  - habu-implement-upstream-defstruct-7cec3e67
---

Problem: slot initforms, constructor defaults, readers, writers, copier behavior, and print dispatch are incomplete. Acceptance: structure construction and mutation follow declared defaults and representation. Files: lib/stdlib.habu:6198-6289, runtime accessor/writer paths. Verify: defstruct regression set for defaults, setf writers, copy-structure, and print-function. Blockers: habu-implement-upstream-defstruct-7cec3e67.
