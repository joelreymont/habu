---
title: Agree on an evaluated is between the load and check.f
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T07:48:23.387762+01:00"
---

Problem: `: X ( -- ) ;  defer D ( -- )  TRUSTED: EV ( -- ) s\" is D\" evaluate ; immediate  s\" EV\" 0 parse-imm  : P ( -- ) ['] X EV ;  P` loads with rc 0, but tools/check.f rejects it with E-MISMATCH 'P ends with [ -- ]'; the installed hb and W192's engine behave the same. Acceptance: the load and check.f give one answer for this program, the one the language's rules give; a test pins it. Depends: none. Claim: unassigned.
