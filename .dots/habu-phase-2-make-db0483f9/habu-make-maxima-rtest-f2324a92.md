---
title: Make maxima-rtest canonical
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.248327+02:00\""
closed-at: "2026-04-04T17:39:46.059099+02:00"
close-reason: "done: wrapper now validates the real first CLI arg, preflights unknown tests from upstream testsuite.lisp before full load, restores CL-USER after foreign loads, and keeps fail-closed loader/diff checks"
blocks:
  - habu-propagate-load-ctx-181876c0
---

Problem: tools/maxima-rtest.lisp still guesses paths and can report success from dirty state. Acceptance: it resolves only through canonical search, validates test names, and fails closed on partial loader state or diff or break outcomes. Files: tools/maxima-rtest.lisp:14-15,42-48,65-83. Verify: tool smoke on valid and invalid test names plus dirty-loader refusal. Blockers: habu-propagate-load-ctx-181876c0.
