---
title: Check a deeply nested stored type in linear time
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T14:18:28.301524+01:00"
---

Problem: src/core/checker.f CAST-INTRO? (:3783) and CAST-MAY-LINEAR? (:3817), reached from STORAGE-TERM-OK? through STORED-TERM-HIT (:3921), walk a layout instance's arguments and then its members, which are built over the same arguments; the walk remembers only the path, so each argument is walked twice per level: storing sdnest<n,sdnest<n,…>> in a TYPED-BUFFER takes 0.56 s at 15 levels, 4.6 s at 18, 18.9 s at 20. Acceptance: the walk expands each instance once per walk; a TYPED-BUFFER of a 300-deep nested type checks in test/structure-decl-suite.f. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-castseen
