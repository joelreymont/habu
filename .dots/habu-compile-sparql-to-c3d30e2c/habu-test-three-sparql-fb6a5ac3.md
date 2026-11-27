---
title: Test three SPARQL rejection and guard gaps
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T21:04:10.858766+01:00"
---

Problem: no test has the "expected BY" rejection (expression.f); none fails if SURE-GIVEN?'s blank-node guard (emit.f) is removed, which would mis-join OPTIONAL rows; none bounds the statement of an OPTIONAL over a union nested in another, which once doubled per level (1.2 GB at depth 20). Acceptance: a test for each, each failing without what it guards. Depends: none.
