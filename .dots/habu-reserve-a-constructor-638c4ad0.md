---
title: Reserve a constructor package name both ways
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T07:07:10.330971+01:00"
---

Problem: a top-level `package QZ-PT` opened before `QZ` declares a public family `pt` silently receives pt's generated words (QZ-PT:MAKE and the rest) and is then closed to its own author (E-CTOR-PROTECTED on a later definition into it); opened after the family it is E-CTOR-PROTECTED at once (/tmp/fable-w189/t1.f, t3.f). Acceptance: both orders give one record at the second of the two names, which says the package name is the family's constructor package; a test pins both orders. Depends: none. Claim: unassigned.
