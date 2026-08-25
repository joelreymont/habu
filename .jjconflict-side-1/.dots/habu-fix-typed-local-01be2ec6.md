---
title: Fix typed-local lint blind spot on group openers
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T20:10:18.659148+02:00"
---

Full context: found by agent livereaders 2026-07-30. tools/typed-local-diff-lint.f reports a FALSE untyped-local finding when a diff changes a locals group's opening line but not its closing :} line - the closer arrives as unchanged context, the lint never recognises the group ended, and the changed opener is judged as if bare. Reproducer: any diff reflowing only the {: line of an existing typed group. The lane worked around it by reflowing the whole group into the change; the lint itself is wrong. Fix the group-tracking state machine to close groups on context :} lines too, add a fixture with a changed opener + unchanged closer (must pass) and a genuinely bare group (must still fail), falsify by reverting the state fix.
