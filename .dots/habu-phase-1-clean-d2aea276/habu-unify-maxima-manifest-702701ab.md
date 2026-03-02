---
title: Unify Maxima manifest
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.085681+02:00"
blocks:
  - habu-delete-maxima-semantic-37a32126
---

Problem: loader, tests, and benches do not share one authoritative Maxima manifest. Acceptance: one manifest defines root, modules, search roots, and provenance. Files: lib/maxima-loader.lisp:6-26,30-159; lib/maxima-post-load.lisp:214-330; tools/maxima-rtest.lisp:1-83; bench/maxima_workload.zig:373-401; lib/stdlib.habu:7319-7347. Verify: loader, test, and bench report the same trusted root/module manifest. Blockers: habu-delete-maxima-semantic-37a32126.
