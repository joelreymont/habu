---
title: Let the run judge past a point the check left to it
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T08:57:34.284612+01:00"
---

Problem: tools/check.f's pre-pass rejection outranks the run even when it follows a top-level token the pre-pass left to the run (an undeclared renderer such as evaluate, a name only rendered text may define), where the walk only guesses how the tokens after it read. On master: 's" : x ;" evaluate {: require :}' loads with E-UNDEFINED at {: 1:20 but check.f gives E-MISSING-SOURCE for ':}' at 1:31; 's" : x ;" evaluate nosuch {: require :}' loads with E-UNDEFINED nosuch 1:20, check.f gives ':}' 1:38; a rendered parsing word (s" : P ( -- ) parse-name 2drop ;" evaluate, then 'P is', 'P postpone', 'P variable' or 'P {:' before definitions) makes the walk misread the definitions after it. Acceptance: the load and check.f (plain and the first --all-errors record) print the same record for each reproducer, and check.f passes where the load passes; a test pins it. Depends: b36a1d34. Claim: unassigned.
