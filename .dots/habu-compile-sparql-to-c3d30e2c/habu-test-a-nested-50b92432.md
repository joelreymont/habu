---
title: Test a nested SPARQL OPTIONAL joined as it is
status: closed
priority: 2
issue-type: task
created-at: "2026-11-27T16:07:11.231235+01:00"
closed-at: "2026-11-27T16:30:02.551839+01:00"
---

Problem: one src/lib/sparql-test.f case nests an OPTIONAL; the join as it is (src/lib/sparql/emit.f AS-IS-ITEM) has none for FILTERs reading outer variables, an empty left side, an inner OPTIONAL joined to the outermost group, a sibling sharing a group's variable, or a walk under !BOUND. Acceptance: each shape has a case whose expected answer is the one the commit before the join as it is gives. Depends: none.
