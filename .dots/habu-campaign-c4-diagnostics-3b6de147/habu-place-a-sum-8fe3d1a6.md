---
title: "Place a sum type payload's arity rejection at its type"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-16T13:39:07.966326+01:00"
closed-at: "2026-11-17T11:32:17.145130+01:00"
---

Problem: a SUMTYPE payload with the wrong number of type arguments (`VARIANT ok option<n,` / `n>`) is rejected 'type family applied to wrong number of arguments' at the sum's name, not at the type; ENUM and STRUCTURE mark the type. Acceptance: the record marks the payload's type in the runner and check.f; a test in the sum type suite fails first. Depends: none. Claim: unassigned.
