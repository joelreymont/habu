---
title: "Place a bad qualified FUNCTION: name at the name"
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T19:29:26.568713+01:00"
---

Problem: for 'FUNCTION: A:B:C getpid ( -- n )' the load reports E-BAD-QUALIFIED-RECORD at ';FUNCTION' (3:1) while tools/check.f --verify-only reports it at the name (2:11); the two loaders disagree. Found by carl-qspell's worker. Acceptance: both report it at the name, the same record. Depends: carl's spelling batch on master. Claim: unassigned.
