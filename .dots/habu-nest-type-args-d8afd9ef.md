---
title: Nest type arguments past 256 in a type
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T13:49:38.610273+01:00"
---

Problem: src/core/structure-decl.f:98 SD-ARG-CAP (256) sizes SD-ARGS, and SD-ARG+ (:371-377) rejects the 257th type argument nested in one type ('application is too deep'). A limit Habu sets on itself. Acceptance: a type nesting more than 256 type arguments declares and checks; a test pins it and fails first. Depends: none. Claim: unassigned.
