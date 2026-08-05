---
title: Fix the stale commit-gate lint name
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T17:53:41.296751+02:00"
---

docs/forth.md § Commit gate names 'host-lint', which does not exist in this tree — a stale reference readers will chase. Point it at the real gate set (the four lints the lanes actually run) or delete the name. Found by the fold2 lane 2026-08-05.
