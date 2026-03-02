---
title: Make maxima-rtest canonical
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.248327+02:00"
blocks:
  - habu-propagate-load-ctx-181876c0
---

Problem: tools/maxima-rtest.lisp still guesses paths and can report success from dirty state. Acceptance: it resolves only through canonical search, validates test names, and fails closed on partial loader state or diff or break outcomes. Files: tools/maxima-rtest.lisp:14-15,42-48,65-83. Verify: tool smoke on valid and invalid test names plus dirty-loader refusal. Blockers: habu-propagate-load-ctx-181876c0.
