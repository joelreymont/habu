---
title: Write an undriven SPARQL union read twice once
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T20:32:37.431297+01:00"
---

Problem: an undriven union the query reads more than once is written, and computed by PostgreSQL, once per read: s-union-250's 21 union texts are byte-equal (src/lib/sparql/emit.f UNION-STEP; walks already share, PLAN-WALKS). Acceptance: an undriven union whose text, its own names aside, equals an earlier one's in the statement, and which reads no outer binding, is written once and read by every use; answers unchanged on sparql-test.f and w3c-test.f. Depends: none.
