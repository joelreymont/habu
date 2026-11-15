---
title: Cut dead and doubled help code
status: closed
priority: 2
issue-type: task
created-at: "2026-11-16T03:11:34.761089+01:00"
closed-at: "2026-11-16T04:07:36.562271+01:00"
---

Problem: src/core/structure-decl.f SD-ARG-KIND's wrap region help is unreachable (a wrap's scope slots are rejected first), and SD-PTR-FIX$ copies NEWTYPE-HELP's name-folding loop. Acceptance: the dead help is gone and one word folds the name for both. Depends: none. Claim: agent=W171 workspace=.jj-ws.noindex/jerry-w171.
