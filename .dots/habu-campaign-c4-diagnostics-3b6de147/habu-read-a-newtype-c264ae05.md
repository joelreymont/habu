---
title: "Read a NEWTYPE's type across lines"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-16T13:39:07.984735+01:00"
closed-at: "2026-11-17T11:32:28.996002+01:00"
---

Problem: `NEWTYPE nq option<n,` / `  n>` is rejected 'unclosed type application' at line 1, while STRUCTURE reads a type across lines. Acceptance: NEWTYPE reads its type as STRUCTURE's FIELD does, across lines; a test fails first. Depends: none. Claim: unassigned.
